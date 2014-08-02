-- | Balanced sets.  This module is the guardian of the core principle
-- of double-entry accounting, which is that all transactions must be
-- balanced.
module Penny.Ents
  (
  -- * Ent
    Ent
  , entQty
  , entCommodity
  , entTrio
  , entMeta
  , Entrio(..)
  , entToTrio
  , rearrange

  -- * Ents
  , Ents
  , entsSeq
  , ents
  , rEnts
  , mapV
  , sequence
  , sequenceR

  -- * Errors
  , Error(..)
  , EntError(..)
  , ErrorCode(..)
  , UnbalancedAtEnd(..)

  -- * Views
  , View
  , viewL
  , viewR
  , allViews
  , unView
  , viewLeft
  , viewCurrent
  , viewRight
  , siblings
  , moveLeft
  , moveRight
  , changeCurrent
  , changeCurrentMeta
  ) where


-- # Imports

import Penny.Balance
import Penny.Common
import Penny.Numbers.Qty
import Data.Monoid
import qualified Penny.Trio as T
import qualified Data.Map as M
import Penny.Numbers.Concrete
import Prelude hiding (negate, sequence)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup
import Data.Either.Combinators
import Data.Sequence (Seq, (|>), (<|), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr
import qualified Data.Foldable as F
import Control.Applicative

-- | Fields in the 'Ent' capture some of the information that was
-- passed along in the 'T.Trio'.  'Entrio' captures the remaining
-- information from the 'T.Trio' that other fields in the 'Ent' does
-- not capture, so that it is possible to reconstruct the original
-- 'T.Trio' from the 'Ent' alone.  Each of the constructors here
-- corresponds to one of the constructors in 'T.Trio'.
data Entrio
  = QC (Either (Polar Period Side) (Polar Comma Side)) Arrangement
  | Q (Either (Polar Period Side) (Polar Comma Side))
  | SC
  | S
  | UC (Either (Unpolar Period) (Unpolar Comma)) Arrangement
  | U (Either (Unpolar Period) (Unpolar Comma))
  | C
  | E
  deriving (Eq, Ord, Show)

-- | Information from a single entry.  Always contains a 'Commodity'
-- and a 'Qty' which holds the quantity information in concrete form.
-- There is also an 'Entrio', which holds the information necessary to
-- rebuild 'T.Trio' from the 'Ent'.
--
-- This is abstract, so only functions in this module may build an 'Ent'.
--
-- There is also arbitrary metadata.  Because 'Ent' is abstract, you
-- cannot build new 'Ent' or change the data in an existing 'Ent',
-- with two exceptions: you can use 'fmap' to change the metadata, and
-- you can use 'rearrange' to change the 'Arrangement' if there is
-- one.  Changing any other data would risk unbalancing the 'Ents'.
data Ent m = Ent
  { entQty :: Qty
  -- ^ Concrete representation of the abstract quantity

  , entCommodity :: Commodity
  , entTrio :: Entrio
  -- ^ Holds information necessary to rebuild the original 'T.Trio';
  -- see 'Entrio'.

  , entMeta :: m
  -- ^ Whatever metadata you wish; typically this will be information
  -- about the posting, such as its account and tags.

  } deriving (Eq, Ord, Show)

instance Functor Ent where
  fmap f e = e { entMeta = f (entMeta e) }

-- | Zero or more 'Ent'.  Together they are balanced; that is, every
-- 'Debit' of a particular 'Commodity' is offset by an equal number of
-- 'Credit's of the same 'Commodity', and vice versa.
--
-- 'Ents' is a 'Monoid'.
newtype Ents m = Ents { entsSeq :: Seq (Ent m)
                      , entsBal :: Balances }
  deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f (Ents sq bl) Ents (fmap (fmap f) sq) bl

instance Monoid (Ents m) where
  mempty = Ents mempty mempty
  mappend (Ents s1 b1) (Ents s2 b2) = Ents (s1 <> s2) (b1 <> b2)

instance F.Foldable Ents where
  foldr f z = F.foldr f z . fmap entMeta . entsSeq

instance Tr.Traversable Ents where
  sequenceA = sequence

-- | Run each action in an 'Ents' from left to right.
sequence :: Applicative f => Ents (f a) -> f (Ents a)
sequence (Ents sq bl) = fmap (flip Ents bl) . go $ sq
  where
    go es = case S.viewl es of
      EmptyL -> pure S.empty
      (Ent q c e m) :< xs ->
        (<|) <$> (Ent q c e <$> m)
             <*> go xs

-- | 'sequence' in reverse; that is, run each action in an 'Ents' from
-- right to left.
sequenceR :: Applicative f => Ents (f a) -> f (Ents a)
sequenceR (Ents sq bl) = fmap (flip Ents bl) . go $ sq
  where
    go es = case S.viewr es of
      EmptyR -> pure S.empty
      xs :> (Ent q c e m) ->
        (flip (|>)) <$> (Ent q c e <$> m) <*> go xs

-- | Change the metadata on each 'Ent', using a function that has a
-- view of the entire 'Ent'.  Like 'fmap' but gives a view of the
-- whole 'Ent' rather than just the metadata.
mapV :: (Ent a -> b) -> Ents a -> Ents b
mapV f (Ents sq bl) = flip Ents bl . go $ sq
  where
    go s = case S.viewl s of
      EmptyL -> S.empty
      w@(Ent q c e _) :< xs -> Ent q c e (f w) <| go xs


entToTrio :: Ent m -> T.Trio
entToTrio (Ent q c t _) = case t of
  QC a ar -> T.QC a c ar
  Q a -> T.Q a
  SC -> T.SC s c
  S -> T.S s
  UC b ar -> T.UC b c ar
  U b -> T.U b
  C -> T.C c
  E -> T.E
  where
    s = case qtySide q of
      Nothing -> error "entToTrio: qty has no side"
      Just sd -> sd

-- | Change the 'Arrangement' in an 'Ent'.  Does nothing if the 'Ent'
-- has no 'Arrangement' to begin with.
rearrange :: Arrangement -> Ent m -> Ent m
rearrange a' e = e { entTrio = e' }
  where
    e' = case entTrio e of
      QC a _ -> QC a a'
      UC b _ -> UC b a'
      x -> x

-- | Different errors that may arise when processing a single 'T.Trio'
-- for conversion to an 'Ent'.
data ErrorCode
  = SCWrongSide
  | SWrongSide
  | CommodityNotFound
  | NoCommoditiesInBalance
  | MultipleCommoditiesInBalance
  | QQtyTooBig
  deriving (Eq, Ord, Show)

-- | An error occurred while attempting to create an 'Ent'.
data EntError = EntError
  { errCode :: ErrorCode
  -- ^ The exact nature of the error.

  , errTrio :: T.Trio
  -- ^ The 'T.Trio' that caused the error.

  , errBalances :: M.Map Commodity (Side, Qty)
  -- ^ The balances that existed at the time the error occurred.
  } deriving (Eq, Ord, Show)

-- | An error arose while attempting to create an 'Ents'.  The error
-- may have arose while processing an individual 'T.Trio' to an 'Ent'.
-- Or, processing of all 'Ent's may have succeeded, but if the total
-- of all the postings is not balanced, an error occurs.
newtype Error
  = Error { unError :: Either EntError UnbalancedAtEnd }
  deriving (Eq, Ord, Show)

-- | The total of all 'Ent' is not balanced.
data UnbalancedAtEnd = UnbalancedAtEnd
  { uaeBalances :: M.Map Commodity (Side, Qty) }
  deriving (Eq, Ord, Show)


procEnt
  :: Balances
  -> (T.Trio, m)
  -> Either EntError (Balances, Ent m)
procEnt bals (tri, mta) = fmap f $ procTrio unbals tri
  where
    unbals = onlyUnbalanced bals
    f (q, cy) = (bals', ent)
      where
        ent = Ent q cy (buildEntrio tri) mta
        bals' = bals <> balance cy q


procEntM
  :: (T.Trio, m)
  -> EitherT EntError (State Balances) (Ent m)
procEntM (tri, mta) = do
  bal <- lift get
  case procEnt bal (tri, mta) of
    Left e -> left e
    Right (bal', r) -> do
      lift $ put bal'
      return r


-- | Only creates an 'Ents' if all the 'T.Trio' are balanced.  See
-- 'T.Trio' for more information on the rules this function follows.
ents
  :: Seq (T.Trio, m)
  -> Either Error (Ents m)
ents ls =
  let (finalEi, finalBal) = flip runState emptyBalances
       . runEitherT . Tr.mapM procEntM $ ls
  in case finalEi of
      Left e -> Left . Error . Left $ e
      Right es
        | M.null unbals -> Right (Ents es)
        | otherwise -> Left . Error . Right . UnbalancedAtEnd $ unbals
        where
          unbals = onlyUnbalanced finalBal


buildEntrio :: T.Trio -> Entrio
buildEntrio t = case t of
  T.QC a _ ar -> QC a ar
  T.Q a -> Q a
  T.SC _ _ -> SC
  T.S _ -> S
  T.UC b _ ar -> UC b ar
  T.U b -> U b
  T.C _ -> C
  T.E -> E

procTrio
  :: M.Map Commodity (Side, Qty)
  -> T.Trio
  -> Either EntError (Qty, Commodity)

procTrio bal trio = case trio of

  T.QC a cy _ -> Right (q, cy)
    where
      q = either polarToQty polarToQty a

  T.Q a -> case singleCommodity of
    Left e -> Left e
    Right (cy, _, _) -> Right (q, cy)
      where
        q = either polarToQty polarToQty a

  T.SC s cy -> case lookupCommodity cy of
    Left e -> Left e
    Right (sBal, q)
      | sBal /= opposite s -> Left $ EntError SCWrongSide trio bal
      | otherwise -> Right (q', cy)
      where
        q' = Qty . negate . unQty $ q

  T.S s -> case singleCommodity of
    Left e -> Left e
    Right (cy, sBal, q)
      | sBal /= opposite s -> Left $ EntError SWrongSide trio bal
      | otherwise -> Right (q', cy)
      where
        q' = Qty . negate . unQty $ q

  T.UC b cy _ -> case lookupCommodity cy of
    Left e -> Left e
    Right (s, _) -> Right (q, cy)
      where
        s' = opposite s
        q = either (unpolarToQty s') (unpolarToQty s') b

  T.U b -> case singleCommodity of
    Left e -> Left e
    Right (cy, s, balQ)
      | abs (unQty q') > abs (unQty balQ) -> Left $ EntError
          QQtyTooBig trio bal
      | otherwise -> Right (q', cy)
      where
        s' = opposite s
        q' = either (unpolarToQty s') (unpolarToQty s') b

  T.C cy -> case lookupCommodity cy of
    Left e -> Left e
    Right (_, balQ) -> Right (q', cy)
      where
        q' = Qty . negate . unQty $ balQ

  T.E -> case singleCommodity of
    Left e -> Left e
    Right (cy, _, balQ) -> Right (q', cy)
      where
        q' = Qty . negate . unQty $ balQ

  where

    -- Looks up a commodity in the given 'Balances'.  Then finds the
    -- requested commodity and returns its balance.  Fails if the
    -- requested commodity is not present.

    -- lookupCommodity :: Commodity -> Either (EntError a b) (Side, Qty)
    lookupCommodity cy = case M.lookup cy bal of
      Nothing -> Left $ EntError CommodityNotFound trio bal
      Just (s, q) -> return (s, q)

    -- Gets a single commodity from the given 'Balances', if it has just
    -- a single commodity.

    -- singleCommodity :: Either (EntError a b) (Commodity, Side, Qty)
    singleCommodity = case M.assocs bal of
      [] -> Left $ EntError NoCommoditiesInBalance trio bal
      (cy, (s, q)):[] -> Right (cy, s, q)
      _ -> Left $ EntError MultipleCommoditiesInBalance trio bal


-- | Creates 'Ents' but, unlike 'ents', never fails.  To make this
-- guarantee, 'rEnts' puts restrictions on its arguments.

rEnts
  :: Side
  -- ^ All postings that you supply are on this 'Side'

  -> Commodity
  -- ^ All postings will have this same 'Commodity'

  -> Arrangement
  -- ^ All postings except for the inferred one will use this
  -- 'Arrangement'

  -> a
  -- ^ Metadata for inferred posting

  -> Seq (Either (Unpolar Period) (Unpolar Comma), a)
  -- ^ Each non-zero member, and its corresponding metadata

  -> Ents a
  -- ^ The resulting 'Ents'.  If the list of 'NZGrouped' was
  -- non-empty, then a single inferred posting is at the end to
  -- balance out the other non-zero postings.  Each 'Ent' has an
  -- 'Entrio' of 'SZC', except for the inferred posting, which is 'E'.

rEnts s cy ar mt ls
  | S.null ls = Ents S.empty
  | otherwise = Ents $ S.zipWith mkEnt qs ls |> inferred
  where
    qs = fmap (mkQ . fst) ls
    mkQ = either (unpolarToQty s) (unpolarToQty s)
    offset = Qty . negate . F.sum . fmap unQty $ qs
    inferred = Ent offset cy E mt
    mkEnt q (ei, m) = Ent q cy (QC ei' ar) m
      where
        ei' = mapBoth (polarizeUnpolar s) (polarizeUnpolar s) ei

-- | A single 'Ent' with information about how it relates to
-- surrounding 'Ent' in the 'Ents'.
data View a = View
  { viewLeft :: Seq (Ent a)
  -- ^ These 'Ent' are to the left of the 'viewCurrent' 'Ent'.  Closer
  -- 'Ent' are at the right end of the 'Seq'.

  , viewCurrent :: Ent a
  -- ^ The 'Ent' you are currently viewing

  , viewRight :: Seq (Ent a)
  -- ^ These 'Ent' are to the right of the 'viewCurrent' 'Ent'.
  -- Closer 'Ent' are at the left end of the 'Seq'.

  } deriving (Eq, Ord, Show)

instance Functor View where
  fmap f (View l c r) = View (fmap (fmap f) l) (fmap f c)
    (fmap (fmap f) r)

instance F.Foldable View where
  foldr f z (View l c r) = F.foldr f z
    (fmap entMeta $ l <> S.singleton c <> r)

instance Tr.Traversable View where
  sequenceA v = View
    <$> trSeq (viewLeft v)
    <*> trEnt (viewCurrent v)
    <*> trSeq (viewRight v)
    where
      trEnt (Ent q c e m) = Ent q c e <$> m
      trSeq sq = case S.viewl sq of
        EmptyL -> pure S.empty
        e :< es -> (<|) <$> trEnt e <*> trSeq es

-- | All 'Ent' that neighbor the 'viewCurrent' 'Ent'.  The list is
-- ordered with all 'Ent' from left to right; the 'viewCurrent' 'Ent'
-- is not in the resulting list.
siblings :: View a -> Seq (Ent a)
siblings (View l _ r) = l <> r

-- | A 'View' of the head 'Ent' in the 'Ents'.  Is 'Nothing' if the
-- 'Ents' is empty.
viewL :: Ents a -> Maybe (View a)
viewL (Ents es) = case S.viewl es of
  EmptyL -> Nothing
  x :< xs -> Just $ View S.empty x xs

-- | A 'View' of the last 'Ent' in the 'Ents'.  Is 'Nothing' if the
-- 'Ents is empty.
viewR :: Ents a -> Maybe (View a)
viewR (Ents es) = case S.viewr es of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x S.empty

-- | A single 'View' for each 'Ent' in the 'Ents'.
allViews :: Ents a -> Seq (View a)
allViews = go S.empty . unEnts
  where
    go l curr = case S.viewl curr of
      EmptyL -> S.empty
      x :< xs -> View l x xs <| go (x <| l) xs


-- | Recreate the 'Ents' from a 'View'.
unView :: View a -> Ents a
unView (View l c r) = Ents $ l <> S.singleton c <> r

-- | Get a new 'View' of the 'Ent' to the left of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the leftmost
-- 'Ent'.
moveLeft :: View a -> Maybe (View a)
moveLeft (View l c r) = case S.viewr l of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x (c <| r)

-- | Get a new 'View' of the 'Ent' to the right of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the
-- rightmost 'Ent'.
moveRight :: View a -> Maybe (View a)
moveRight (View l c r) = case S.viewl r of
  EmptyL -> Nothing
  x :< xs -> Just $ View (l |> c) x xs

-- | Returns a 'View' whose 'viewCurrent' 'Ent' has been transformed
-- by the given function.
changeCurrent :: (Ent a -> Ent a) -> View a -> View a
changeCurrent f (View l c r) = View l (f c) r

-- | Returns a 'View' where the metadata in the 'viewCurrent' 'Ent'
-- has been transformed by the given function.
changeCurrentMeta :: (a -> a) -> View a -> View a
changeCurrentMeta f (View l c r) = View l (fmap f c) r

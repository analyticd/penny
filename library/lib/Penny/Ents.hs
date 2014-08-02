-- | Sets of entries.  Though every 'Ent' in an 'Ents' is valid, the
-- entire set is not necessarily balanced; for balanced sets, see
-- "Penny.Balanced".
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
  , entsBal
  , appendEnt
  , rEnts
  , mapV
  , sequence
  , sequenceR

  -- * Errors
  , EntError(..)
  , ErrorCode(..)

  -- * Views
  , View
  , viewL
  , viewR
  , allViews
  , unView
  , viewLeft
  , viewCurrent
  , viewRight
  , viewBalance
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
data Ents m = Ents { entsSeq :: Seq (Ent m)
                   , entsBal :: Balances }
  deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f (Ents sq bl) = Ents (fmap (fmap f) sq) bl

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

  , errBalances :: Imbalances
  -- ^ The balances that existed at the time the error occurred.
  } deriving (Eq, Ord, Show)

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


appendEnt
  :: Ents m
  -> (T.Trio, m)
  -> Either EntError (Ents m)
appendEnt (Ents s b) p = case procEnt b p of
  Left e -> Left e
  Right (b', e) -> Right $ Ents (s |> e) b'


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
  :: Imbalances
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
    lookupCommodity cy = case M.lookup cy . unImbalances $ bal of
      Nothing -> Left $ EntError CommodityNotFound trio bal
      Just (s, q) -> return (s, q)

    -- Gets a single commodity from the given 'Balances', if it has just
    -- a single commodity.

    -- singleCommodity :: Either (EntError a b) (Commodity, Side, Qty)
    singleCommodity = case M.assocs . unImbalances $ bal of
      [] -> Left $ EntError NoCommoditiesInBalance trio bal
      (cy, (s, q)):[] -> Right (cy, s, q)
      _ -> Left $ EntError MultipleCommoditiesInBalance trio bal


-- | Creates 'Ents' but, unlike 'appendEnt', never fails.  To make this
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
  | S.null ls = Ents mempty mempty
  | otherwise = Ents sq bl
  where
    sq = S.zipWith mkEnt qs ls |> inferred
    bl = mconcat . F.toList . fmap (uncurry balance)
      . fmap (\e -> (entCommodity e, entQty e)) $ sq
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

  , viewBalance :: Balances
  -- ^ The balance of all 'Ent' in the 'View'.

  } deriving (Eq, Ord, Show)

instance Functor View where
  fmap f (View l c r b) = View (fmap (fmap f) l) (fmap f c)
    (fmap (fmap f) r) b

instance F.Foldable View where
  foldr f z (View l c r _) = F.foldr f z
    (fmap entMeta $ l <> S.singleton c <> r)

instance Tr.Traversable View where
  sequenceA v = View
    <$> trSeq (viewLeft v)
    <*> trEnt (viewCurrent v)
    <*> trSeq (viewRight v)
    <*> pure (viewBalance v)
    where
      trEnt (Ent q c e m) = Ent q c e <$> m
      trSeq sq = case S.viewl sq of
        EmptyL -> pure S.empty
        e :< es -> (<|) <$> trEnt e <*> trSeq es

-- | All 'Ent' that neighbor the 'viewCurrent' 'Ent'.  The list is
-- ordered with all 'Ent' from left to right; the 'viewCurrent' 'Ent'
-- is not in the resulting list.
siblings :: View a -> Seq (Ent a)
siblings (View l _ r _) = l <> r

-- | A 'View' of the head 'Ent' in the 'Ents'.  Is 'Nothing' if the
-- 'Ents' is empty.
viewL :: Ents a -> Maybe (View a)
viewL (Ents es b) = case S.viewl es of
  EmptyL -> Nothing
  x :< xs -> Just $ View S.empty x xs b

-- | A 'View' of the last 'Ent' in the 'Ents'.  Is 'Nothing' if the
-- 'Ents is empty.
viewR :: Ents a -> Maybe (View a)
viewR (Ents es b) = case S.viewr es of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x S.empty b

-- | A single 'View' for each 'Ent' in the 'Ents'.
allViews :: Ents a -> Seq (View a)
allViews (Ents sq b) = go S.empty sq
  where
    go l curr = case S.viewl curr of
      EmptyL -> S.empty
      x :< xs -> View l x xs b <| go (x <| l) xs


-- | Recreate the 'Ents' from a 'View'.
unView :: View a -> Ents a
unView (View l c r b) = Ents (l <> S.singleton c <> r) b

-- | Get a new 'View' of the 'Ent' to the left of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the leftmost
-- 'Ent'.
moveLeft :: View a -> Maybe (View a)
moveLeft (View l c r b) = case S.viewr l of
  EmptyR -> Nothing
  xs :> x -> Just $ View xs x (c <| r) b

-- | Get a new 'View' of the 'Ent' to the right of the 'viewCurrent'
-- 'Ent'.  Is 'Nothing' if the input 'View' is already of the
-- rightmost 'Ent'.
moveRight :: View a -> Maybe (View a)
moveRight (View l c r b) = case S.viewl r of
  EmptyL -> Nothing
  x :< xs -> Just $ View (l |> c) x xs b

-- | Returns a 'View' whose 'viewCurrent' 'Ent' has been transformed
-- by the given function.
changeCurrent :: (Ent a -> Ent a) -> View a -> View a
changeCurrent f (View l c r b) = View l (f c) r b

-- | Returns a 'View' where the metadata in the 'viewCurrent' 'Ent'
-- has been transformed by the given function.
changeCurrentMeta :: (a -> a) -> View a -> View a
changeCurrentMeta f (View l c r b) = View l (fmap f c) r b

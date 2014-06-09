-- | Balanced sets.  This module is the guardian of the core principle
-- of double-entry accounting, which is that all transactions must be
-- balanced.
module Penny.Lincoln.Ents
  (
  -- * Ent
    Ent
  , entConcrete
  , entCommodity
  , entTrio
  , entMeta
  , Entrio(..)
  , entToTrio
  , rearrange

  -- * Ents
  , Ents
  , unEnts
  , ents
  , rEnts

  -- * Errors
  , Error(..)
  , EntError(..)
  , ErrorCode(..)
  , UnbalancedAtEnd(..)

  -- * Views
  , View
  , view
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

import Penny.Lincoln.Balance
import Penny.Lincoln.Common
import Penny.Lincoln.Decimal
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Either
import qualified Data.Map as M
import Prelude hiding (exponent, negate)
import qualified Penny.Lincoln.Trio as T
import Data.Monoid

-- | Fields in the 'Ent' capture some of the information that was
-- passed along in the 'T.Trio'.  'Entrio' captures the remaining
-- information from the 'T.Trio' that other fields in the 'Ent' does
-- not capture, so that it is possible to reconstruct the original
-- 'T.Trio' from the 'Ent' alone.  Each of the constructors here
-- corresponds to one of the constructors in 'T.Trio'.
data Entrio
  = SZC NZGrouped Arrangement
  | SZ NZGrouped
  | SC
  | S
  | ZC NZGrouped Arrangement
  | Z NZGrouped
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
data Ent a = Ent

  { entConcrete :: Qty
  -- ^ The concrete representation of the abstract non-zero quantity.

  , entCommodity :: Commodity

  , entTrio :: Entrio
  -- ^ Holds information necessary to rebuild the original 'T.Trio';
  -- see 'Entrio'.

  , entMeta :: a
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
newtype Ents a = Ents { unEnts :: [Ent a] }
  deriving (Eq, Ord, Show)

instance Functor Ents where
  fmap f = Ents . map (fmap f) . unEnts

instance Monoid (Ents a) where
  mempty = Ents []
  mappend (Ents x) (Ents y) = Ents $ x ++ y

entToTrio :: Ent a -> T.Trio
entToTrio (Ent q cy entro _) = tri
  where
    tri = case entro of
      SZC nzg ar -> T.SZC s nzg cy ar
      SZ nzg -> T.SZ s nzg
      SC -> T.SC s cy
      S -> T.S s
      ZC nzg ar -> T.ZC nzg cy ar
      Z nzg -> T.Z nzg
      C -> T.C cy
      E -> T.E
    s = case qtySide q of
      Nothing -> error "ents: qty has no side"
      Just sd -> sd

-- | Change the 'Arrangement' in an 'Ent'.  Does nothing if the 'Ent'
-- has no 'Arrangement' to begin with.
rearrange :: Arrangement -> Ent a -> Ent a
rearrange a' e = e { entTrio = e' }
  where
    e' = case entTrio e of
      SZC g _ -> SZC g a'
      ZC g _ -> ZC g a'
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
newtype Error = Error { unError :: Either EntError UnbalancedAtEnd }
  deriving (Eq, Ord, Show)

-- | The total of all 'Ent' is not balanced.
data UnbalancedAtEnd = UnbalancedAtEnd
  { uaeBalances :: M.Map Commodity (Side, Qty) }
  deriving (Eq, Ord, Show)

procEnt
  :: Balances
  -> (T.Trio, a)
  -> Either EntError (Balances, Ent a)
procEnt bals (tri, mta) = fmap f $ procTrio unbals tri
  where
    unbals = onlyUnbalanced bals
    f (q, cy) = (bals', ent)
      where
        ent = Ent q cy (buildEntrio tri) mta
        bals' = bals <> balance cy q

procEntM :: (T.Trio, a) -> EitherT EntError (State Balances) (Ent a)
procEntM (tri, mta) = do
  bal <- lift get
  case procEnt bal (tri, mta) of
    Left e -> left e
    Right (bal', r) -> do
      lift $ put bal'
      return r

-- | Only creates an 'Ents' if all the 'T.Trio' are balanced.  See
-- 'T.Trio' for more information on the rules this function follows.
ents :: [(T.Trio, a)] -> Either Error (Ents a)
ents ls =
  let (finalEi, finalBal) = flip runState emptyBalances
       . runEitherT . mapM procEntM $ ls
  in case finalEi of
      Left e -> Left . Error . Left $ e
      Right es
        | M.null unbals -> Right (Ents es)
        | otherwise -> Left . Error . Right . UnbalancedAtEnd $ unbals
        where
          unbals = onlyUnbalanced finalBal

buildEntrio :: T.Trio -> Entrio
buildEntrio t = case t of
  T.SZC _ nzg _ ar -> SZC nzg ar
  T.SZ _ nzg -> SZ nzg
  T.SC _ _ -> SC
  T.S _ -> S
  T.ZC nzg _ ar -> ZC nzg ar
  T.Z nzg -> Z nzg
  T.C _ -> C
  T.E -> E

procTrio
  :: M.Map Commodity (Side, Qty)
  -> T.Trio
  -> Either EntError (Qty, Commodity)
procTrio bal trio = case trio of

  T.SZC s nzg cy _ -> Right (q, cy)
    where
      prms = Params (sign s) (coefficient nzg) (exponent nzg)
      q = Qty $ normal prms

  T.SZ s nzg -> case singleCommodity of
    Left e -> Left e
    Right (cy, _, _) -> Right (q, cy)
      where
        prms = Params (sign s) (coefficient nzg) (exponent nzg)
        q = Qty $ normal prms

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

  T.ZC nzg cy _ -> case lookupCommodity cy of
    Left e -> Left e
    Right (s, _) -> Right (q, cy)
      where
        q = Qty $ normal pms
        pms = Params (sign . opposite $ s) (coefficient nzg)
                (exponent nzg)

  T.Z nzg -> case singleCommodity of
    Left e -> Left e
    Right (cy, s, balQ)
      | abs (unQty q') > abs (unQty balQ) -> Left $ EntError
          QQtyTooBig trio bal
      | otherwise -> Right (q', cy)
      where
        q' = Qty . normal $ Params (sign . opposite $ s)
          (coefficient nzg) (exponent nzg)

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

    lookupCommodity :: Commodity -> Either EntError (Side, Qty)
    lookupCommodity cy = case M.lookup cy bal of
      Nothing -> Left $ EntError CommodityNotFound trio bal
      Just (s, q) -> return (s, q)

    -- Gets a single commodity from the given 'Balances', if it has just
    -- a single commodity.

    singleCommodity :: Either EntError (Commodity, Side, Qty)
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

  -> [(NZGrouped, a)]
  -- ^ Each non-zero number, and its corresponding metadata

  -> Ents a
  -- ^ The resulting 'Ents'.  If the list of 'NZGrouped' was
  -- non-empty, then a single inferred posting is at the end to
  -- balance out the other non-zero postings.  Each 'Ent' has an
  -- 'Entrio' of 'SZC', except for the inferred posting, which is 'E'.

rEnts s cy ar mt ls
  | null ls = Ents []
  | otherwise = Ents $ zipWith mkEnt qs ls ++ [inferred]
  where
    qs = map mkQ . map fst $ ls
    mkQ nzg = Qty . normal $ Params (sign s) (coefficient nzg)
      (exponent nzg)
    offset = Qty . negate . sum . map unQty $ qs
    inferred = Ent offset cy E mt
    mkEnt q (nzg, m) = Ent q cy (SZC nzg ar) m

data View a = View
  { viewLeft :: [Ent a]
  , viewCurrent :: Ent a
  , viewRight :: [Ent a]
  } deriving (Eq, Ord, Show)

instance Functor View where
  fmap f (View l c r) = View (map (fmap f) l) (fmap f c)
    (map (fmap f) r)

siblings :: View a -> [Ent a]
siblings (View l _ r) = reverse l ++ r

view :: Ents a -> Maybe (View a)
view (Ents es) = case es of
  [] -> Nothing
  x:xs -> Just $ View [] x xs

allViews :: Ents a -> [View a]
allViews = go [] . unEnts
  where
    go l curr = case curr of
      [] -> []
      x:xs -> View l x xs : go (x:l) xs

unView :: View a -> Ents a
unView (View l c r) = Ents $ reverse l ++ c:r

moveLeft :: View a -> Maybe (View a)
moveLeft (View l c r) = case l of
  [] -> Nothing
  x:xs -> Just $ View xs x (c:r)

moveRight :: View a -> Maybe (View a)
moveRight (View l c r) = case r of
  [] -> Nothing
  x:xs -> Just $ View (c:l) x xs

changeCurrent :: (Ent a -> Ent a) -> View a -> View a
changeCurrent f (View l c r) = View l (f c) r

changeCurrentMeta :: (a -> a) -> View a -> View a
changeCurrentMeta f (View l c r) = View l (fmap f c) r

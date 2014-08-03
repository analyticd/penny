module Penny.Ent
  ( Ent
  , entQty
  , entCommodity
  , entTrio
  , entMeta
  , Entrio(..)
  , entToTrio
  , ErrorCode(..)
  , EntError(..)
  , mkEnt
  , mkQCEnt
  , mkEEnt
  , rearrange
  ) where

import Penny.Common
import Penny.Numbers.Qty
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup
import qualified Penny.Trio as T
import Penny.Balance
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T

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

instance F.Foldable Ent where
  foldr f z (Ent _ _ _ m) = f m z

instance T.Traversable Ent where
  sequenceA (Ent q c e m) = fmap (Ent q c e) m

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

mkEnt
  :: (T.Trio, m)
  -> Imbalances
  -> Either EntError (Ent m)
mkEnt (tri, mt) imb = case procTrio imb tri of
  Left e -> Left e
  Right (q, c) -> Right (Ent q c (buildEntrio tri) mt)

mkQCEnt
  :: Either (Polar Period Side) (Polar Comma Side)
  -> Commodity
  -> Arrangement
  -> m
  -> Ent m
mkQCEnt ei cy ar mt = Ent q cy (buildEntrio tri) mt
  where
    q = either polarToQty polarToQty ei
    tri = T.QC ei cy ar

mkEEnt
  :: (Commodity, Qty)
  -> m
  -> Ent m
mkEEnt (cy, q) mt = Ent q cy E mt

-- | Change the 'Arrangement' in an 'Ent'.  Does nothing if the 'Ent'
-- has no 'Arrangement' to begin with.
rearrange :: Arrangement -> Ent m -> Ent m
rearrange a' e = e { entTrio = e' }
  where
    e' = case entTrio e of
      QC a _ -> QC a a'
      UC b _ -> UC b a'
      x -> x

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

    -- lookupCommodity :: Commodity -> Either EntError (Side, Qty)
    lookupCommodity cy = case M.lookup cy . unImbalances $ bal of
      Nothing -> Left $ EntError CommodityNotFound trio bal
      Just nz@(NonZero _ _ s) ->
        return (s, paramsToQty . nonZeroToQtyParams $ nz)

    -- Gets a single commodity from the given 'Balances', if it has just
    -- a single commodity.

    -- singleCommodity :: Either EntError (Commodity, Side, Qty)
    singleCommodity = case M.assocs . unImbalances $ bal of
      [] -> Left $ EntError NoCommoditiesInBalance trio bal
      (cy, nz@(NonZero _ _ s)):[] ->
        Right (cy, s, paramsToQty . nonZeroToQtyParams $ nz)
      _ -> Left $ EntError MultipleCommoditiesInBalance trio bal



module Penny.Ent where

import Penny.Common
import Penny.Numbers.Qty
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Abstract.RadGroup
import qualified Penny.Trio as T
import Penny.Balance
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Penny.Numbers.Abstract.Polar

-- | Information from a single entry.

data Ent m = Ent
  { entQty :: Qty
  -- ^ Concrete representation of the abstract quantity

  , entCommodity :: Commodity
  , entMeta :: m
  -- ^ Whatever metadata you wish; typically this will be information
  -- about the posting, such as its account and tags.

  } deriving (Eq, Ord, Show)

instance Functor Ent where
  fmap f e = e { entMeta = f (entMeta e) }

instance F.Foldable Ent where
  foldr f z (Ent _ _ m) = f m z

instance T.Traversable Ent where
  sequenceA (Ent q c m) = fmap (Ent q c) m


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

mkEnt
  :: (T.Trio, m)
  -> Imbalances
  -> Either EntError (Ent m)
mkEnt (tri, mt) imb = case procTrio imb tri of
  Left e -> Left e
  Right (q, c) -> Right (Ent q c mt)

mkQCEnt
  :: Either (Polar Period Side) (Polar Comma Side)
  -> Commodity
  -> m
  -> Ent m
mkQCEnt ei cy mt = Ent q cy mt
  where
    q = either polarToQty polarToQty ei

mkEEnt
  :: (Commodity, Qty)
  -> m
  -> Ent m
mkEEnt (cy, q) mt = Ent q cy mt

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




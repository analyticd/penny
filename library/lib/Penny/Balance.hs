module Penny.Balance where

import qualified Data.Map as M
import Data.Maybe
import Penny.Common
import Penny.Numbers.Qty
import Penny.Numbers.Concrete
import qualified Data.Foldable as F
import Data.Monoid
import qualified Deka.Dec as D
import Control.Arrow (second)
import Deka.Native.Abstract hiding (Exponent)

newtype Balances = Balances { unBalances :: M.Map Commodity Qty }
  deriving (Eq, Ord, Show)

instance Monoid Balances where
  mempty = emptyBalances
  mappend (Balances x) (Balances y) = Balances $ M.unionWith f x y
    where
      f (Qty a) (Qty b) = Qty $ a + b

emptyBalances :: Balances
emptyBalances = Balances M.empty

balance :: Commodity -> Qty -> Balances
balance c = Balances . M.singleton c

addEntry
  :: Commodity
  -> Qty
  -> Balances
  -> Balances
addEntry c q = Balances . M.alter f c . unBalances
  where
    f v = case v of
      Nothing -> Just q
      Just (Qty l) -> Just . Qty $ l + unQty q

isBalanced :: Balances -> Bool
isBalanced = F.all (D.isZero . unConcrete . unQty) . unBalances

data NonZero = NonZero
  { nzCoeff :: NE Novem Decem
  , nzExp :: Exponent
  , nzSide :: Side
  } deriving (Eq, Ord, Show)

newtype Imbalances = Imbalances
  { unImbalances :: M.Map Commodity NonZero }
  deriving (Eq, Ord, Show)

qtyParamsToNonZero :: QtyParams -> Maybe NonZero
qtyParamsToNonZero (QtyParams may e) = case may of
  Nothing -> Nothing
  Just (s, nd) -> Just $ NonZero nd e s

nonZeroToQtyParams :: NonZero -> QtyParams
nonZeroToQtyParams (NonZero nd e s) = QtyParams (Just (s, nd)) e

-- | Removes all balanced commodity-qty pairs from the map.
onlyUnbalanced :: Balances -> Imbalances
onlyUnbalanced = Imbalances . M.fromList . mapMaybe f
  . map (second qtyToParams) . M.toList . unBalances
  where
    f (c, p) = fmap (\nz -> (c, nz)) $ qtyParamsToNonZero p



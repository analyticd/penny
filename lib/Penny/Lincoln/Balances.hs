module Penny.Lincoln.Balances where

import Penny.Lincoln.Amount
import qualified Data.Foldable as F
import Penny.Lincoln.Decimal
import Penny.Commodity
import Penny.Lincoln.Qty
import qualified Data.Map as M
import Data.Monoid

newtype Balances = Balances (M.Map Commodity Qty)
  deriving (Eq, Ord, Show)

instance Monoid Balances where
  mempty = Balances M.empty
  mappend (Balances x) (Balances y) = Balances $ M.unionWith (+) x y

c'Balances'Amount :: Amount -> Balances
c'Balances'Amount (Amount c q) = Balances $ M.singleton c q

addAmountToBalances :: Amount -> Balances -> Balances
addAmountToBalances (Amount c q) (Balances m) = Balances . M.alter f c $ m
  where
    f v = case v of
      Nothing -> Just q
      Just l -> Just $ q + l

isBalanced :: Balances -> Bool
isBalanced (Balances m) = F.all isZero m
  where
    isZero (Qty (Decimal signif _)) = signif == 0

newtype Imbalances = Imbalances (M.Map Commodity QtyNonZero)
  deriving (Eq, Ord, Show)

c'Imbalances'Amount :: Amount -> Imbalances
c'Imbalances'Amount = c'Imbalances'Balances . c'Balances'Amount

c'Imbalances'Balances :: Balances -> Imbalances
c'Imbalances'Balances (Balances m)
  = Imbalances
  . M.mapMaybe qtyToQtyNonZero $ m

c'Balances'Imbalances :: Imbalances -> Balances
c'Balances'Imbalances (Imbalances m)
  = Balances . fmap qtyNonZeroToQty $ m

instance Monoid Imbalances where
  mempty = Imbalances M.empty
  mappend x y =
    c'Imbalances'Balances $ mappend
      (c'Balances'Imbalances x) (c'Balances'Imbalances y)

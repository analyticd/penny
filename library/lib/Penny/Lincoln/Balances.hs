module Penny.Lincoln.Balances where

import qualified Data.Foldable as F
import Penny.Lincoln.Decimal
import Penny.Lincoln.Commodity
import Penny.Lincoln.Qty
import qualified Data.Map as M
import Data.Monoid

newtype Balances = Balances (M.Map Commodity Qty)
  deriving (Eq, Ord, Show)

instance Monoid Balances where
  mempty = Balances M.empty
  mappend (Balances x) (Balances y) = Balances $ M.unionWith (+) x y

balancesFromPair :: Qty -> Commodity -> Balances
balancesFromPair q c = Balances $ M.singleton c q

addEntryToBalances :: Qty -> Commodity -> Balances -> Balances
addEntryToBalances q c (Balances m) = Balances . M.alter f c $ m
  where
    f v = case v of
      Nothing -> Just q
      Just l -> Just $ q + l

isBalanced :: Balances -> Bool
isBalanced (Balances m) = F.all isZero m
  where
    isZero (Qty (Decimal signif _)) = signif == 0

newtype Imbalances = Imbalances (M.Map Commodity DecNonZero)
  deriving (Eq, Ord, Show)

imbalancesFromPair :: Qty -> Commodity -> Imbalances
imbalancesFromPair q c = balancesToImbalances $ balancesFromPair q c

balancesToImbalances :: Balances -> Imbalances
balancesToImbalances (Balances m)
  = Imbalances
  . M.mapMaybe f $ m
  where
    f (Qty q) = decimalToDecNonZero q

imbalancesToBalances :: Imbalances -> Balances
imbalancesToBalances (Imbalances m)
  = Balances . fmap (Qty . decNonZeroToDecimal) $ m

instance Monoid Imbalances where
  mempty = Imbalances M.empty
  mappend x y =
    balancesToImbalances $ mappend
      (imbalancesToBalances x) (imbalancesToBalances y)

module Penny.Lincoln.Balance where

import qualified Data.Map as M
import Penny.Lincoln.Common
import Penny.Lincoln.Decimal
import qualified Data.Foldable as F

newtype Balances = Balances { unBalances :: M.Map Commodity Qty }
  deriving (Eq, Ord, Show)

emptyBalances :: Balances
emptyBalances = Balances M.empty

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
isBalanced = F.all (isZero . unQty) . unBalances

-- | Removes all balanced commodity-qty pairs from the map.
onlyUnbalanced :: Balances -> Balances
onlyUnbalanced = Balances . M.filter (not . isZero . unQty)
  . unBalances

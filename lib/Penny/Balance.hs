module Penny.Balance where

import qualified Data.Map as M
import Penny.Common
import Penny.Numbers.Qty
import Penny.Numbers.Concrete
import qualified Data.Foldable as F
import Data.Monoid
import qualified Deka.Dec as D

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

-- | Removes all balanced commodity-qty pairs from the map.
onlyUnbalanced :: Balances -> M.Map Commodity (Side, Qty)
onlyUnbalanced = M.mapMaybe f . unBalances
  where
    f q = case qtySide q of
      Nothing -> Nothing
      Just s -> Just (s, q)


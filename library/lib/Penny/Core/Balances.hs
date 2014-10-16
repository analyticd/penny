module Penny.Core.Balances where

import qualified Data.Map as M
import qualified Penny.Core.Commodity as Commodity
import qualified Penny.Core.Qty as Qty
import qualified Data.Foldable as F
import Data.Monoid

newtype T = T { toMap :: M.Map Commodity.T Qty.T }
  deriving (Eq, Ord, Show)

instance Monoid T where
  mempty = T M.empty
  mappend (T x) (T y) = T $ M.unionWith f x y
    where
      f (Qty.T a) (Qty.T b) = Qty.T $ a + b

empty :: T
empty = T M.empty

fromPair :: Commodity.T -> Qty.T -> T
fromPair c q = T $ M.singleton c q

addEntry :: Commodity.T -> Qty.T -> T -> T
addEntry c q = T . M.alter f c . toMap
  where
    f v = case v of
      Nothing -> Just q
      Just (Qty.T l) -> Just . Qty.T $ l + Qty.toConcrete q

isBalanced :: T -> Bool
isBalanced = F.all Qty.isZero . toMap

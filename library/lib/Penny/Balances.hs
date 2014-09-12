module Penny.Balances where

import qualified Data.Map as M
import qualified Penny.Commodity as Cy
import qualified Penny.Qty as Q
import qualified Penny.Concrete as C
import qualified Data.Foldable as F
import qualified Deka.Dec as D
import Data.Monoid

newtype T = T { toMap :: M.Map Cy.T Q.T }
  deriving (Eq, Ord, Show)

instance Monoid T where
  mempty = T M.empty
  mappend (T x) (T y) = T $ M.unionWith f x y
    where
      f (Q.T a) (Q.T b) = Q.T $ a + b

empty :: T
empty = T M.empty

fromPair :: Cy.T -> Q.T -> T
fromPair c q = T $ M.singleton c q

addEntry :: Cy.T -> Q.T -> T -> T
addEntry c q = T . M.alter f c . toMap
  where
    f v = case v of
      Nothing -> Just q
      Just (Q.T l) -> Just . Q.T $ l + Q.toConcrete q

isBalanced :: T -> Bool
isBalanced = F.all (D.isZero . C.toDec .Q.toConcrete) . toMap

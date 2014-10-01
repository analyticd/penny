module Penny.Core.Qty where

import qualified Penny.Core.Concrete as Concrete
import qualified Penny.Core.Cement as Cement

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)

fromConcrete :: Concrete.T -> T
fromConcrete = T

toCement :: T -> Cement.T
toCement = Concrete.toCement . toConcrete

fromCement :: Cement.T -> T
fromCement = fromConcrete . Concrete.fromCement

simpleCompare :: T -> T -> Ordering
simpleCompare (T x) (T y) = Concrete.simpleCompare x y

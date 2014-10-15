module Penny.Core.Qty where

import qualified Penny.Core.Concrete as Concrete
import qualified Penny.Core.Cement as Cement

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)

fromConcrete :: Concrete.T -> T
fromConcrete = T

toCement :: T -> Cement.T
toCement = Cement.fromConcrete . toConcrete

fromCement :: Cement.T -> T
fromCement = fromConcrete . Cement.toConcrete

compareEquiv :: T -> T -> Ordering
compareEquiv (T x) (T y) = Concrete.compareEquiv x y

module Penny.Core.Qty where

import qualified Penny.Core.Concrete as Concrete
import qualified Penny.Core.Cement as Cement

-- | A Qty is concrete, meaning that you can perform arithmetic with
-- it.  However, it is not intended to be represented for the user
-- on-screen.
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

isZero :: T -> Bool
isZero = Concrete.isZero . toConcrete

module Penny.Lincoln.Decimal.Components where

import Penny.Lincoln.Natural
import Deka.Native.Abstract hiding (Exponent(..))
import Prelude hiding (exponent)

newtype Exponent = Exponent { unExponent :: NonNegative }
  deriving (Eq, Show, Ord)

class HasExponent a where
  exponent :: a -> Exponent

class HasCoefficient a where
  coefficient :: a -> Coefficient

class HasDecuple a where
  decuple :: a -> Decuple

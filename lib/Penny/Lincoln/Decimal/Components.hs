-- | Basic components of all numbers.
module Penny.Lincoln.Decimal.Components where

import Penny.Lincoln.Natural
import Deka.Native.Abstract hiding (Exponent(..))
import Prelude hiding (exponent)

-- | Exponents.  Unlike exponents in Deka, Penny does not use
-- positive exponents because there is no unambiguous way to
-- represent them using ordinary notation.  All exponents are either
-- negative or zero.  'Exponent' holds all exponents as a
-- 'NonNegative', which represents the absolute value of the actual
-- exponent.
newtype Exponent = Exponent { unExponent :: NonNegative }
  deriving (Eq, Show, Ord)

class HasExponent a where
  exponent :: a -> Exponent

class HasCoefficient a where
  coefficient :: a -> Coefficient

-- | Things that are non-zero have a 'Decuple' in their
-- 'Coefficient'.
class HasDecuple a where
  decuple :: a -> Decuple

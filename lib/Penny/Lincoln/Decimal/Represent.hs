module Penny.Lincoln.Decimal.Represent where

import Penny.Lincoln.Decimal.Native
import Penny.Lincoln.Decimal.Rep
import Deka.Native
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Decimal.Lane
import Prelude hiding (exponent)

-- | Represents a number, without any digit grouping.

ungrouped
  :: (HasCoefficient a, HasExponent a, Laned a)
  => a
  -> Rep b
ungrouped = undefined

ungroupedZero :: Exponent -> Zero a
ungroupedZero = undefined

ungroupedNonZero
  :: Exponent
  -> Side
  -> Decuple
  -> Quant a
ungroupedNonZero = undefined



-- | Represents a number, with digit grouping.  Rules for digit
-- grouping:
--
-- * Digits to the left of the radix are grouped only if there are
-- at least five digits.
--
-- * Digits to the right of the radix are never grouped.
--
-- The unit type is used as the groupng character; use 'fmap' to
-- insert your preferred grouping character.

grouped
  :: (HasCoefficient a, HasExponent a)
  => a
  -> Rep ()
grouped = undefined

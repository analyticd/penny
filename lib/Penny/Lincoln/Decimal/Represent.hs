module Penny.Lincoln.Decimal.Represent where

import Penny.Lincoln.Decimal.Native
import Penny.Lincoln.Decimal.Rep
import Deka.Native
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Decimal.Lane
import Prelude hiding (exponent)

-- | Represents a number, without any digit grouping.

ungrouped
  :: (HasExponent a, Laned a)
  => a
  -> Rep b
ungrouped a = case lane a of
  Center -> RZero $ ungroupedZero (exponent a)
  NonCenter (s, d) -> RQuant $ ungroupedNonZero (exponent a) s d

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
  :: (HasExponent a, Laned a)
  => a
  -> Rep ()
grouped a = case lane a of
  Center -> RZero $ ungroupedZero (exponent a)
  NonCenter (s, d) -> RQuant $ groupedNonZero (exponent a) s d

groupedNonZero
  :: Exponent
  -> Side
  -> Decuple
  -> Quant ()
groupedNonZero = undefined

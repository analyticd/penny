module Penny.Lincoln.DecNonZero where

import Penny.Lincoln.Decimal
import Penny.Lincoln.NonZero
import Penny.Lincoln.Natural

-- | Decimals whose significand is never zero.

data DecNonZero = DecNonZero !NonZero Unsigned
  deriving (Eq, Ord, Show)

decNonZeroToDecimal :: DecNonZero -> Decimal
decNonZeroToDecimal (DecNonZero nz u) = Decimal (nonZeroToInteger nz) u

decimalToDecNonZero :: Decimal -> Maybe DecNonZero
decimalToDecNonZero (Decimal signif expt) = case integerToNonZero signif of
  Nothing -> Nothing
  Just nz -> Just $ DecNonZero nz expt

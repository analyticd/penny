module Penny.Lincoln.Decimal.Abstract where

import Penny.Lincoln.Decimal.Zero
import Penny.Lincoln.Decimal.Masuno
import Penny.Lincoln.Decimal.Frac
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Decimal.Lane
import Penny.Lincoln.Decimal.Side
import Prelude hiding (exponent)

-- | An abstract non-zero number.
data NonZero
  = NZMasuno Masuno
  | NZFrac Frac
  deriving (Eq, Ord, Show)

instance HasDecuple NonZero where
  decuple z = case z of
    NZMasuno w -> decuple w
    NZFrac f -> decuple f

instance HasCoefficient NonZero where
  coefficient z = case z of
    NZMasuno w -> coefficient w
    NZFrac f -> coefficient f

instance HasExponent NonZero where
  exponent z = case z of
    NZMasuno w -> exponent w
    NZFrac f -> exponent f

-- | An abstract non-zero number, along with a 'Side' to describe
-- whether it is a 'Debit' or 'Credit'.
data Figure = Figure
  { figSide :: Side
  , figNonZero :: NonZero
  } deriving (Eq, Ord, Show)

instance Sided Figure where
  side = figSide

instance Laned Figure where
  lane f = NonCenter (figSide f, decuple . figNonZero $ f)

instance HasDecuple Figure where
  decuple = decuple . figNonZero

instance HasCoefficient Figure where
  coefficient = coefficient . figNonZero

instance HasExponent Figure where
  exponent = exponent . figNonZero

-- | Abstract representation of a number.  Contains the number
-- itself as well as information about whether the number, if a
-- 'Figure', is a 'Debit' or 'Credit'.
data Rep
  = RFigure Figure
  -- ^ Non-zero numbers.  These are necessarily a 'Debit' or
  -- 'Credit', which is stored in the 'Figure'.
  | RZero Zero
  -- ^ Zero numbers.  Not all 'Zero' are the same, as @0.0@ is not
  -- the same as @0.00@ (preserving this information is necessary so
  -- taht some operations obey the monoid laws.)  Zeroes do not have
  -- a 'Side'.
  deriving (Eq, Ord, Show)

instance Laned Rep where
  lane r = case r of
    RFigure f -> lane f
    RZero _ -> Center

instance HasCoefficient Rep where
  coefficient a = case a of
    RFigure f -> coefficient f
    RZero z -> coefficient z

instance HasExponent Rep where
  exponent a = case a of
    RFigure f -> exponent f
    RZero z -> exponent z

-- | What radix character and grouping character to use.
data RadGroup
  = PeriodComma
  -- ^ Period radix, comma grouping
  | PeriodSpace
  -- ^ Period radix, space grouping
  | PeriodThinSpace
  -- ^ Period radix, thin space grouping
  | CommaPeriod
  -- ^ Comma radix, period grouping
  | CommaSpace
  -- ^ Comma radix, space grouping
  | CommaThinSpace
  -- ^ Comma radix, thin space grouping
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Abstract representation of a number, along with what characters
-- to use for the radix point and the digit grouping character.
data Abstract = Abstract
  { absRep :: Rep
  , absRadGroup :: RadGroup
  } deriving (Eq, Ord, Show)

instance Laned Abstract where
  lane = lane . absRep

instance HasCoefficient Abstract where
  coefficient = coefficient . absRep

instance HasExponent Abstract where
  exponent = exponent . absRep

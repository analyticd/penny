module Penny.Lincoln.Decimal.Abstract where

import Penny.Lincoln.Decimal.Zero
import Penny.Lincoln.Decimal.Masuno
import Penny.Lincoln.Decimal.Frac
import Prelude hiding (exponent)

-- | An abstract non-zero number.
data NonZero
  = NZMasuno Masuno
  | NZFrac Frac
  deriving (Eq, Ord, Show)

-- | An abstract non-zero number, along with data that determines
-- its side.  For an amount, this will be a Side; for a Price, this
-- will be a PosNeg.
data Figure a = Figure
  { figPolarity :: a
  , figNonZero :: NonZero
  } deriving (Eq, Ord, Show)

instance Functor Figure where
  fmap f (Figure p n) = Figure (f p) n

-- | Abstract representation of a number.  Contains the number
-- itself as well as information about the polarity of the number,
-- if it is not zero.
data Rep a
  = RFigure (Figure a)
  -- ^ Non-zero numbers.  These are necessarily a 'Debit' or
  -- 'Credit', which is stored in the 'Figure'.
  | RZero Zero
  -- ^ Zero numbers.  Not all 'Zero' are the same, as @0.0@ is not
  -- the same as @0.00@ (preserving this information is necessary so
  -- taht some operations obey the monoid laws.)  Zeroes do not have
  -- a 'Side'.
  deriving (Eq, Ord, Show)

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
-- to use for the radix point and the digit grouping character.  Is
-- parameterized on the polarity.
data Abstract a = Abstract
  { absRep :: Rep a
  , absRadGroup :: RadGroup
  } deriving (Eq, Ord, Show)


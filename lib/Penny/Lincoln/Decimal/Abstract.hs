module Penny.Lincoln.Decimal.Abstract where

import Penny.Lincoln.Decimal.Zero
import Penny.Lincoln.Decimal.Masuno
import Penny.Lincoln.Decimal.Frac
import Penny.Lincoln.Decimal.Components
import Prelude hiding (exponent)

-- | An abstract non-zero number.
data NonZero
  = NZMasuno Masuno
  | NZFrac Frac
  deriving (Eq, Ord, Show)

instance HasExponent NonZero where
  exponent x = case x of
    NZMasuno m -> exponent m
    NZFrac f -> exponent f

instance HasCoefficient NonZero where
  coefficient x = case x of
    NZMasuno m -> coefficient m
    NZFrac f -> coefficient f

-- | An abstract non-zero number, along with data that determines
-- its side.  For an amount, this will be a Side; for a Price, this
-- will be a PosNeg.
data Figure a = Figure
  { figPolarity :: a
  , figNonZero :: NonZero
  } deriving (Eq, Ord, Show)

instance Wrapped Figure where
  unwrap = figPolarity

instance HasExponent (Figure a) where
  exponent = exponent . figNonZero

instance HasCoefficient (Figure a) where
  coefficient = coefficient . figNonZero

instance Signed a => Signed (Figure a) where
  sign = sign . figPolarity

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

instance MaybeWrapped Rep where
  maybeUnwrap r = case r of
    RFigure f -> Just $ unwrap f
    RZero _ -> Nothing

instance HasCoefficient (Rep a) where
  coefficient x = case x of
    RFigure f -> coefficient f
    RZero z -> coefficient z

instance HasExponent (Rep a) where
  exponent x = case x of
    RFigure f -> exponent f
    RZero z -> exponent z

instance Signed a => Signed (Rep a) where
  sign x = case x of
    RFigure f -> sign f
    RZero _ -> Sign0

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

-- | An abstract non-zero number, together with the characters to use
-- for the radix point and digit grouping character.

data NZGrouped = NZGrouped
  { nzgNonZero :: NonZero
  , nzgRadGroup :: RadGroup
  } deriving (Eq, Ord, Show)

instance HasExponent NZGrouped where
  exponent = exponent . nzgNonZero

instance HasCoefficient NZGrouped where
  coefficient = coefficient . nzgNonZero

-- | Abstract representation of a number, along with what characters
-- to use for the radix point and the digit grouping character.  Is
-- parameterized on the polarity.
data Abstract a = Abstract
  { absRep :: Rep a
  , absRadGroup :: RadGroup
  } deriving (Eq, Ord, Show)

instance MaybeWrapped Abstract where
  maybeUnwrap = maybeUnwrap . absRep

instance HasExponent (Abstract a) where
  exponent = exponent . absRep

instance HasCoefficient (Abstract a) where
  coefficient = coefficient . absRep

instance Signed a => Signed (Abstract a) where
  sign = sign . absRep

{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Decimal where

import Control.Lens (makeLenses)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty
import Prelude hiding (length)

import Penny.NonNegative
import Penny.NonZero
import qualified Penny.NonZero as NonZero
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos

-- | Numbers represented exponentially.  In @Exponential c p@, the
-- value of the number is @c * 10 ^ (-1 * naturalToInteger p)@.
--
-- The 'Eq' and 'Ord' are derived.
-- Therefore, for example, @3.5 == 3.500@ is 'False'.
data Exponential c = Exponential
  { _coefficient :: !c
  -- ^ The significant digits; also known as the significand or the mantissa.
  , _power :: !NonNegative
  -- ^ The power of ten.
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance PrettyVal a => PrettyVal (Exponential a)

makeLenses ''Exponential

type Decimal = Exponential Integer

pole'Decimal :: Decimal -> Maybe Pole
pole'Decimal (Exponential c _) = integerPole c

-- | @increaseExponent d e@ returns a 'Decimal' @d'@ whose exponent is
-- equal to @e@; if the exponent of @d@ is greater than or equal to
-- @e@, does nothing.
increaseExponent
  :: NonNegative
  -> Exponential Integer
  -> Exponential Integer
increaseExponent u (Exponential m e) = Exponential m' e'
  where
    (m', e') = case subt u e of
      Nothing -> (m, e)
      Just diff -> (m * 10 ^ c'Integer'NonNegative diff, u)

-- | Equalizes the exponents on two decimals.
--
-- Let:
--
-- > Decimal mx ex = x
-- > Decimal my ey = y
-- > (x'@(Decimal mx' ex'), y'@(Decimal my' ey')) = equalizeExponents x y
--
-- Then the following properties hold:
--
-- > ex' == ey'
-- > x' == x || y' == y
-- > ex' >= ex
-- > ey' >= ey
-- > abs mx' >= abs mx
-- > abs my' >= abs my
equalizeExponents
  :: Exponential Integer
  -> Exponential Integer
  -> (Exponential Integer, Exponential Integer)
equalizeExponents x@(Exponential _ ex) y@(Exponential _ ey)
  | ex > ey = (x, increaseExponent ex y)
  | otherwise = (increaseExponent ey x, y)

instance Num (Exponential Integer) where
  x + y = Exponential (mx' + my') ex
    where
      (Exponential mx' ex, Exponential my' _) = equalizeExponents x y
  x - y = Exponential (mx' - my') ex
    where
      (Exponential mx' ex, Exponential my' _) = equalizeExponents x y
  (Exponential mx ex) * (Exponential my ey) = Exponential (mx * my) (ex `add` ey)
  negate (Exponential mx ex) = Exponential (Prelude.negate mx) ex
  abs (Exponential mx ex) = Exponential (abs mx) ex
  signum (Exponential mx _) = Exponential (signum mx) zero
  fromInteger i = Exponential i zero

-- | Decimals whose significand is never zero.
type DecNonZero = Exponential NonZero

decNonZeroToDecimal :: DecNonZero -> Decimal
decNonZeroToDecimal (Exponential nz u) = Exponential (c'Integer'NonZero nz) u

decimalToDecNonZero :: Decimal -> Maybe DecNonZero
decimalToDecNonZero (Exponential signif expt) = case c'NonZero'Integer signif of
  Nothing -> Nothing
  Just nz -> Just $ Exponential nz expt

-- | Decimals that are unsigned; they may be zero.
type DecUnsigned = Exponential NonNegative

-- | Decimals that are positive; they may not be zero.
type DecPositive = Exponential Positive

-- | Decimals whose significand is always zero.
type DecZero = Exponential ()

-- | Transforms a 'DecPositive' to a 'DecNonZero', while applying the
-- appropriate sign.
c'DecNonZero'DecPositive
  :: Pole
  -> DecPositive
  -> DecNonZero
c'DecNonZero'DecPositive pm (Exponential sig expt)
  = Exponential (changeSign . c'NonZero'Positive $ sig) expt
  where
    changeSign
      | pm == positive = id
      | otherwise = NonZero.negate

-- * Representations

-- Let t = number of trailing significand digits,
-- and e = size of exponent.
--
-- t >= e: decimal is greater than zero
-- t < e: decimal is less than zero.
-- e - t - 1 == number of leading fractional zeroes

{-

S  Significand
LS Length of significand
LM Length of significand - 1
E  Exponent
P  Representation
LL Number of digits to left of radix
LZ Number of leading zeroes on the right of radix
LR Number of digits to right of radix

S       LS LM E   P             LL  LZ  LR

12345   5  4  0   12345         5   0   0
12345   5  4  3   12.345        2   0   3
12345   5  4  5   0.12345       0   0   5
12345   5  4  7   0.0012345     0   2   5
1       1  0  0   1             1   0   0
1       1  0  3   0.001         0   2   1

There are two possible scenarios: (1) LL is positive.  In that case
LZ is always zero and LR might be zero or positive.  (2) LL is zero.
In that case LR is positive and LZ might be zero or positive.

-}

integerToInt :: Integer -> Int
integerToInt x
  | x < fromIntegral (minBound :: Int) = error "integer too small"
  | x > fromIntegral (maxBound :: Int) = error "integer too large"
  | otherwise = fromIntegral x


decomposeDecUnsigned
  :: DecUnsigned
  -> Either DecZero DecPositive
decomposeDecUnsigned (Exponential m e) = case c'Positive'NonNegative m of
  Nothing -> Left (Exponential () e)
  Just m' -> Right (Exponential m' e)

stripDecimalSign
  :: Decimal
  -> Either DecZero (DecPositive, Pole)
stripDecimalSign (Exponential m e) = case Pos.stripIntegerSign m of
  Nothing -> Left (Exponential () e)
  Just (p, pm) -> Right (Exponential p e, pm)

stripNonZeroSign
  :: DecNonZero
  -> (DecPositive, Pole)
stripNonZeroSign (Exponential nz ex)
  = (Exponential (c'Positive'NonZero nz) ex, nonZeroSign nz)


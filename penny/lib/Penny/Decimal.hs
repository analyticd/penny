{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Penny.Decimal
  ( -- * Decimal number types
    Exponential(..)
  , coefficient
  , power
  , Decimal
  , DecUnsigned
  , DecPositive
  , DecZero
  , DecNonZero

  -- * Querying decimal properties
  , pole'Decimal
  , poleDecNonZero

  -- * Manipulating exponents
  -- ** Increase
  , increaseExponent
  , increaseExponentUnsigned
  , increaseExponentPositive
  -- ** Equalize
  , equalizeExponents
  , equalizeExponentsUnsigned
  , equalizeExponentsPositive

  -- * Comparisons
  , cmpDecimal
  , cmpUnsigned
  , cmpPositive

  -- * Arithmetic
  , addDecPositive

  -- * Conversions
  , decNonZeroToDecimal
  , decimalToDecNonZero
  , c'DecNonZero'DecPositive
  , decomposeDecUnsigned
  , stripDecimalSign
  , stripNonZeroSign
  , magnitude
  ) where

import Data.Data (Data)
import Control.Lens (makeLenses)
import qualified Control.Lens as Lens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import Prelude hiding (length)

import Penny.NonNegative
import qualified Penny.NonNegative as NN
import Penny.NonZero
import qualified Penny.NonZero as NonZero
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos

-- | Numbers represented exponentially.  In @Exponential c p@, the
-- value of the number is @c * 10 ^ (-1 * naturalToInteger p)@.
--
-- There are no instances for 'Eq' or 'Ord'.  This is intentional.
-- There do not seem to be any agreed upon laws for 'Eq', but some
-- suggest that if @x == y@, then @f x == f y@ for all @f@.  This law
-- easily holds if 'Eq' is derived.  However, if 'Eq' is derived, then
-- (for example) @3.5 == 3.500@ is 'False'.  That is not necessarily
-- intuitive behavior, but it is the only correct behavior if
-- 'Exponential' is going to have an 'Eq' instance.  (An alternative
-- would be to define the 'Eq' instance so that @3.5 == 3.500@.
-- However, that would violate the given law, because @show 3.5@ would
-- not be the same as @show 3.500@.)
--
--  Therefore, 'Exponential' has no 'Eq' or 'Ord' instance at all.
-- So, if you need to do something like put 'Exponential' as the key
-- for a map, simply remove the two fields and put them into a tuple.
--
-- If you want to compare 'Exponential' so that @3.5@ does equal
-- @3.500@ (and so that ordinal comparisons are similar) see
-- 'cmpDecimal', 'cmpUnsigned', and 'cmpPositive'.
data Exponential c = Exponential
  { _coefficient :: !c
  -- ^ The significant digits; also known as the significand or the mantissa.
  , _power :: !NonNegative
  -- ^ The power of ten.
  } deriving (Show, Functor, Foldable, Traversable, Generic, Data)

instance PrettyVal a => PrettyVal (Exponential a)

makeLenses ''Exponential

-- | Decimals that may be any value.
type Decimal = Exponential Integer

-- | Decimals that are unsigned; they may be zero.
type DecUnsigned = Exponential NonNegative

-- | Decimals that are positive; they may not be zero.
type DecPositive = Exponential Positive

-- | Decimals whose significand is always zero.
type DecZero = Exponential ()

-- | Decimals whose significand is never zero.
type DecNonZero = Exponential NonZero

pole'Decimal :: Decimal -> Maybe Pole
pole'Decimal (Exponential c _) = integerPole c

-- | @increaseExponent d e@ returns a 'Decimal' @d'@ whose exponent is
-- equal to @e@; if the exponent of @d@ is greater than or equal to
-- @e@, does nothing.
increaseExponent
  :: NonNegative
  -> Decimal
  -> Decimal
increaseExponent u (Exponential m e) = Exponential m' e'
  where
    (m', e') = case subt u e of
      Nothing -> (m, e)
      Just diff -> (m * 10 ^ c'Integer'NonNegative diff, u)

-- | Like 'increaseExponent', but for 'Exponential' 'NonNegative'.
increaseExponentUnsigned
  :: NonNegative
  -> DecUnsigned
  -> DecUnsigned
increaseExponentUnsigned u (Exponential m e) = Exponential m' e'
  where
    (m', e') = case subt u e of
      Nothing -> (m, e)
      Just diff -> (m `NN.mult` (NN.ten `NN.pow` diff), u)

-- | Like 'increaseExponent', but for 'Exponential' 'Positive'.
increaseExponentPositive
  :: NonNegative
  -> DecPositive
  -> DecPositive
increaseExponentPositive u (Exponential m e) = Exponential m' e'
  where
    (m', e') = case subt u e of
      Nothing -> (m, e)
      Just diff -> (m `Pos.mult` (Pos.ten `Pos.pow` diff), u)

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
  :: Decimal
  -> Decimal
  -> (Decimal, Decimal)
equalizeExponents x@(Exponential _ ex) y@(Exponential _ ey)
  | ex > ey = (x, increaseExponent ex y)
  | otherwise = (increaseExponent ey x, y)


-- | Like 'equalizeExponents' but for 'Exponential' 'Unsigned'.
equalizeExponentsUnsigned
  :: DecUnsigned
  -> DecUnsigned
  -> (DecUnsigned, DecUnsigned)
equalizeExponentsUnsigned x@(Exponential _ ex) y@(Exponential _ ey)
  | ex > ey = (x, increaseExponentUnsigned ex y)
  | otherwise = (increaseExponentUnsigned ey x, y)

-- | Like 'equalizeExponents' but for 'Exponential' 'Positive'.
equalizeExponentsPositive
  :: DecPositive
  -> DecPositive
  -> (DecPositive, DecPositive)
equalizeExponentsPositive x@(Exponential _ ex) y@(Exponential _ ey)
  | ex > ey = (x, increaseExponentPositive ex y)
  | otherwise = (increaseExponentPositive ey x, y)

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

-- | Compare the coefficients of two 'Exponential' 'Integer', after
-- equalizing their exponents.
cmpDecimal
  :: (Integer -> Integer -> a)
  -> Exponential Integer
  -> Exponential Integer
  -> a
cmpDecimal f ex ey = f (_coefficient ex') (_coefficient ey')
  where
    (ex', ey') = equalizeExponents ex ey

-- | Compare the coefficients of two 'DecUnsigned'
-- after equalizing their exponents.
cmpUnsigned
  :: (NonNegative -> NonNegative -> a)
  -> Exponential NonNegative
  -> Exponential NonNegative
  -> a
cmpUnsigned f ex ey = f (_coefficient ex') (_coefficient ey')
  where
    (ex', ey') = equalizeExponentsUnsigned ex ey

-- | Compare the coefficients of two 'DecPositive'
-- after equalizing their exponents.
cmpPositive
  :: (Positive -> Positive -> a)
  -> DecPositive
  -> DecPositive
  -> a
cmpPositive f ex ey = f (_coefficient ex') (_coefficient ey')
  where
    (ex', ey') = equalizeExponentsPositive ex ey

-- | Adds two 'DecPositive', after equalizing their exponents.
addDecPositive :: DecPositive -> DecPositive -> DecPositive
addDecPositive x y = Exponential (Pos.add xC yC) e'
  where
    (Exponential xC e', Exponential yC _) =
      equalizeExponentsPositive x y

poleDecNonZero :: Lens.Lens' DecNonZero Pole
poleDecNonZero = coefficient . NonZero.pole

decNonZeroToDecimal :: DecNonZero -> Decimal
decNonZeroToDecimal (Exponential nz u) = Exponential (c'Integer'NonZero nz) u

decimalToDecNonZero :: Decimal -> Maybe DecNonZero
decimalToDecNonZero (Exponential signif expt) = case c'NonZero'Integer signif of
  Nothing -> Nothing
  Just nz -> Just $ Exponential nz expt

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

magnitude :: Decimal -> DecUnsigned
magnitude = fmap NN.stripSign

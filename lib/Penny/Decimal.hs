{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Penny.Decimal
  (
  -- * Decimal types and classes
    Exponential(..)
  , coefficient
  , power
  , Decimal
  , HasDecimal(..)
  , HasExponent(..)
  , DecNonZero
  , HasDecNonZero(..)
  , DecUnsigned
  , HasDecUnsigned(..)
  , DecPositive
  , HasDecPositive(..)
  , DecZero
  , HasDecZero(..)

  -- * Exponent manipulations
  , increaseExponent
  , equalizeExponents

  -- * Changing decimal forms
  , stripDecimalSign
  , stripNonZeroSign
  , decomposeDecUnsigned
  , decNonZeroToDecimal
  , decimalToDecNonZero
  , c'DecNonZero'DecPositive

  -- * Representing decimals
  --
  -- | To group the results of these functions, consult
  -- 'Penny.Representation.groupBrimUngrouped'.
  , repUngroupedDecimal
  , repUngroupedDecNonZero
  , repUngroupedDecUnsigned
  , repUngroupedDecZero
  , repUngroupedDecPositive
  , repDigits
  ) where

import Control.Lens
import Penny.Natural
import Penny.NonZero
import Penny.Representation
import Control.Monad (join)
import qualified Data.Sequence as S
import Data.Monoid
import Penny.Offset
import Penny.PluMin
import Data.List (genericSplitAt, genericReplicate)
import Penny.Semantic

-- | Numbers represented exponentially.  In @Exponential c p@, the
-- value of the number is @c * 10 ^ (-1 * naturalToInteger p)@.
data Exponential c = Exponential
  { _coefficient :: !c
  -- ^ The significant digits; also known as the significand or the mantissa.
  , _power :: !Unsigned
  -- ^ The power of ten.
  } deriving Show

makeLenses ''Exponential

type Decimal = Exponential Integer

-- | Class for things that can be converted to a 'Decimal'.
class HasDecimal a where
  toDecimal :: a -> Decimal

instance (HasDecZero n, HasDecPositive o, Signed p)
  => HasDecimal (CenterOrOffCenter n o p) where
  toDecimal (Center n) = toDecimal . toDecZero $ n
  toDecimal (OffCenter o p) = changeSign (toDecimal . toDecPositive $ o)
    where
      changeSign = case sign p of
        Plus -> id
        Minus -> negate

class HasExponent a where
  toExponent :: a -> Unsigned

-- | @increaseExponent d e@ returns a 'Decimal' @d'@ whose exponent is
-- equal to @e@; if the exponent of @d@ is greater than or equal to
-- @e@, does nothing.
increaseExponent
  :: Pow c
  => Unsigned
  -> Exponential c
  -> Exponential c
increaseExponent u (Exponential m e) = Exponential m' e'
  where
    (m', e') = case subt u e of
      Nothing -> (m, e)
      Just diff -> (raise m diff, u)


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
  :: (Pow a, Pow b)
  => Exponential a
  -> Exponential b
  -> (Exponential a, Exponential b)
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
  negate (Exponential mx ex) = Exponential (negate mx) ex
  abs (Exponential mx ex) = Exponential (abs mx) ex
  signum (Exponential mx _) = Exponential (signum mx) (toUnsigned Zero)
  fromInteger i = Exponential i (toUnsigned Zero)

-- | Decimals whose significand is never zero.
type DecNonZero = Exponential NonZero

class HasDecNonZero a where
  toDecNonZero :: a -> DecNonZero

instance HasOffset (Exponential NonZero) where
  offset (Exponential sig expt) = Exponential (offset sig) expt

decNonZeroToDecimal :: DecNonZero -> Decimal
decNonZeroToDecimal (Exponential nz u) = Exponential (nonZeroToInteger nz) u

decimalToDecNonZero :: Decimal -> Maybe DecNonZero
decimalToDecNonZero (Exponential signif expt) = case integerToNonZero signif of
  Nothing -> Nothing
  Just nz -> Just $ Exponential nz expt


-- | Decimals that are unsigned; they may be zero.
type DecUnsigned = Exponential Unsigned

instance Eq (Semantic (Exponential Unsigned)) where
  Semantic x == Semantic y = x' == y'
    where
      (Exponential x' _, Exponential y' _) = equalizeExponents x y

instance Ord (Semantic (Exponential Unsigned)) where
  compare (Semantic x) (Semantic y) = compare x' y'
    where
      (Exponential x' _, Exponential y' _) = equalizeExponents x y


class HasDecUnsigned a where
  toDecUnsigned :: a -> DecUnsigned

-- | Decimals that are positive; they may not be zero.
type DecPositive = Exponential Positive

instance Eq (Semantic (Exponential Positive)) where
  Semantic x == Semantic y = x' == y'
    where
      (Exponential x' _, Exponential y' _) = equalizeExponents x y

instance Ord (Semantic (Exponential Positive)) where
  compare (Semantic x) (Semantic y) = compare x' y'
    where
      (Exponential x' _, Exponential y' _) = equalizeExponents x y

class HasDecPositive a where
  toDecPositive :: a -> DecPositive

-- | Decimals whose significand is always zero.
type DecZero = Exponential ()

class HasDecZero a where
  toDecZero :: a -> DecZero

instance HasDecimal DecZero where
  toDecimal (Exponential () expt) = Exponential 0 expt

instance HasDecimal DecPositive where
  toDecimal (Exponential sig expt) = Exponential (naturalToInteger sig) expt

instance HasExponent (NilUngrouped r) where
  toExponent nu = case nu of
    NUZero _ Nothing -> toUnsigned Zero
    NUZero _ (Just (_, Nothing)) -> toUnsigned Zero
    NUZero _ (Just (_, Just (_, zs))) -> next (lengthUnsigned zs)
    NURadix _ _ zs -> next (lengthUnsigned zs)

instance HasDecZero (NilUngrouped r) where
  toDecZero x = Exponential () (toExponent x)

instance HasExponent (Nil r) where
  toExponent nil = case nil of
    NilU nu -> toExponent nu
    NilG ng -> toExponent ng

instance HasDecZero (Nil r) where
  toDecZero x = case x of
    NilU nu -> Exponential () (toExponent nu)
    NilG ng -> Exponential () (toExponent ng)

instance HasExponent (NilGrouped r) where
  toExponent (NilGrouped _ _ _ zs1 _ _ zs2 zss) =
      next . next . add (lengthUnsigned zs1) . add (lengthUnsigned zs2)
      . lengthUnsigned . join
      . fmap (\(_, _, sq) -> Zero <| sq) $ zss

instance HasDecZero (NilGrouped r) where
  toDecZero x = Exponential () (toExponent x)

-- | Strips the sign from the 'DecNonZero'.
instance HasDecPositive DecNonZero where
  toDecPositive (Exponential sig expt) =
    Exponential (nonZeroToPositive sig) expt

instance HasDecPositive (BrimUngrouped r) where

  toDecPositive (BUGreaterThanOne nv ds1 Nothing)
    = Exponential (novDecsToPositive nv ds1) (toUnsigned Zero)

  toDecPositive (BUGreaterThanOne nv ds1 (Just (_, ds2)))
    = Exponential (novDecsToPositive nv (ds1 <> ds2))
                  (lengthUnsigned ds2)

  toDecPositive (BULessThanOne _ _ zs1 nv ds)
    = Exponential (novDecsToPositive nv ds)
                  (add (lengthUnsigned zs1) . next . lengthUnsigned $ ds)

instance HasDecPositive (BrimGrouped r) where
  toDecPositive = toDecPositive . ungroupBrimGrouped

instance HasDecPositive (Brim a) where
  toDecPositive (BrimGrouped a) = toDecPositive a
  toDecPositive (BrimUngrouped a) = toDecPositive a

-- | Transforms a 'DecPositive' to a 'DecNonZero', while applying the
-- appropriate sign.
c'DecNonZero'DecPositive
  :: PluMin
  -> DecPositive
  -> DecNonZero
c'DecNonZero'DecPositive pm (Exponential sig expt)
  = Exponential (c'NonZero'Positive pm sig) expt

-- * Representations

repUngroupedDecimal
  :: Radix r
  -> Decimal
  -> CenterOrOffCenter (NilUngrouped r) (BrimUngrouped r) PluMin
repUngroupedDecimal rdx d = case stripDecimalSign d of
  Left zero -> Center (repUngroupedDecZero rdx zero)
  Right (pos, pm) -> OffCenter (repUngroupedDecPositive rdx pos) pm

repUngroupedDecNonZero
  :: Radix r
  -> DecNonZero
  -> (BrimUngrouped r, PluMin)
repUngroupedDecNonZero rdx nz = (repUngroupedDecPositive rdx dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecUnsigned
  :: Radix r
  -> DecUnsigned
  -> CenterOrOffCenter (NilUngrouped r) (BrimUngrouped r) ()
repUngroupedDecUnsigned rdx uns = case decomposeDecUnsigned uns of
  Left z -> Center (repUngroupedDecZero rdx z)
  Right p -> OffCenter (repUngroupedDecPositive rdx p) ()

-- Primitive grouping functions

stripDecimalSign
  :: Decimal
  -> Either DecZero (DecPositive, PluMin)
stripDecimalSign (Exponential m e) = case stripIntegerSign m of
  Nothing -> Left (Exponential () e)
  Just (p, pm) -> Right (Exponential p e, pm)

stripNonZeroSign
  :: DecNonZero
  -> (DecPositive, PluMin)
stripNonZeroSign (Exponential nz ex)
  = (Exponential (nonZeroToPositive nz) ex, nonZeroSign nz)

decomposeDecUnsigned
  :: DecUnsigned
  -> Either DecZero DecPositive
decomposeDecUnsigned (Exponential m e) = case unsignedToPositive m of
  Nothing -> Left (Exponential () e)
  Just m' -> Right (Exponential m' e)

repUngroupedDecZero :: Radix r -> DecZero -> NilUngrouped r
repUngroupedDecZero rdx (Exponential () expt) = case unsignedToPositive expt of
  Nothing -> NUZero Zero Nothing
  Just pos -> NUZero Zero (Just (rdx, Just (Zero, rest)))
    where
      rest = case prev pos of
        Nothing -> S.empty
        Just ps -> go ps (S.singleton Zero)
      go ps acc = case prev ps of
        Nothing -> acc
        Just ps' -> go ps' (Zero <| acc)

repUngroupedDecPositive :: Radix r -> DecPositive -> BrimUngrouped r
repUngroupedDecPositive rdx (Exponential sig expt)
  = repDigits rdx (positiveDigits sig) expt

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

repDigits
  :: Radix r
  -> (D9, [D9z])
  -> Unsigned
  -> BrimUngrouped r
repDigits rdx (d1, dr) expt
  = case diffUnsigned (next $ lengthUnsigned dr) expt of
      Equal -> BULessThanOne (Just Zero) rdx S.empty d1 (S.fromList dr)
      LeftBiggerBy l -> BUGreaterThanOne d1 leftDigs rightDigs
        where
          (leftDigs, rightDigs) = case prev l of
            Nothing -> (S.empty, Just (rdx, S.fromList dr))
            Just c -> (S.fromList beg, Just (rdx, S.fromList end))
              where
                (beg, end) = genericSplitAt (naturalToInteger c) dr
      RightBiggerBy r -> BULessThanOne (Just Zero) rdx zs d1 (S.fromList dr)
        where
          zs = S.fromList . flip genericReplicate Zero
            . naturalToInteger $ r


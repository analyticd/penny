module Penny.Lincoln.Decimal
  (
  -- * Decimal types and classes
    Decimal(..)
  , HasDecimal(..)
  , HasExponent(..)
  , Semantic(..)
  , DecNonZero(..)
  , HasDecNonZero(..)
  , DecUnsigned(..)
  , HasDecUnsigned(..)
  , DecPositive(..)
  , HasDecPositive(..)
  , DecZero(..)
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
  -- 'Penny.Number.Rep.groupBrimUngrouped'.
  , repUngroupedDecimal
  , repUngroupedDecNonZero
  , repUngroupedDecUnsigned
  , repUngroupedDecZero
  , repUngroupedDecPositive
  , repDigits
  ) where

import Penny.Number.Natural
import Penny.Lincoln.NonZero
import Penny.Number.Rep
import Control.Monad (join)
import Data.Sequence ((<|))
import qualified Data.Sequence as S
import Data.Monoid
import Penny.Offset
import Penny.PluMin
import Data.List (genericSplitAt, genericReplicate)

-- | Decimal numbers.  The precision is limited only by the machine's
-- available memory (or, more realistically, by how big a number the
-- machine can handle before grinding to unusable slowness.)  The 'Eq'
-- and 'Ord' instances are derived; therefore:
--
-- >>> let twoPointZero = Decimal 20 . toPositive $ D9'1
-- >>> let twoPointZeroZero = Decimal 200 . toPositive $ D9'2
-- >>> twoPointZero == twoPointZeroZero
-- False
-- >>> twoPointZeroZero > twoPointZero
-- True

data Decimal
  = Decimal !Integer !Unsigned
  -- ^ @Decimal a b@, where
  --
  -- @a@ is the significand, and
  --
  -- @b@ is the exponent, such that the value of the number is
  --
  -- > a * 10 ^ (-1 * naturalToInteger b)
  --
  -- For example, @2.00@ is equal to
  --
  -- > Decimal 200 . toPositive $ D9'2
  deriving (Eq, Ord, Show)

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

instance Signed p => HasDecimal (NilOrBrimPolar r p) where
  toDecimal (NilOrBrimPolar x) = toDecimal x

class HasExponent a where
  toExponent :: a -> Unsigned

-- | @increaseExponent d e@ returns a 'Decimal' @d'@ whose exponent is
-- equal to @e@; if the exponent of @d@ is greater than or equal to
-- @e@, does nothing.
increaseExponent :: Unsigned -> Decimal -> Decimal
increaseExponent u (Decimal m e) = Decimal m' e'
  where
    (m', e') = case subt u e of
      Nothing -> (m, e)
      Just diff -> (m * 10 ^ naturalToInteger diff, u)


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
equalizeExponents :: Decimal -> Decimal -> (Decimal, Decimal)
equalizeExponents x@(Decimal _ ex) y@(Decimal _ ey)
  | ex > ey = (x, increaseExponent ex y)
  | otherwise = (increaseExponent ey x, y)

instance Num Decimal where
  x + y = Decimal (mx' + my') ex
    where
      (Decimal mx' ex, Decimal my' _) = equalizeExponents x y
  x - y = Decimal (mx' - my') ex
    where
      (Decimal mx' ex, Decimal my' _) = equalizeExponents x y
  (Decimal mx ex) * (Decimal my ey) = Decimal (mx * my) (ex `add` ey)
  negate (Decimal mx ex) = Decimal (negate mx) ex
  abs (Decimal mx ex) = Decimal (abs mx) ex
  signum (Decimal mx _) = Decimal (signum mx) (toUnsigned Zero)
  fromInteger i = Decimal i (toUnsigned Zero)

-- | Compares 'Decimal' based on semantics rather than actual values;
-- does this by first equalizing exponents before performing
-- comparisons.
newtype Semantic = Semantic Decimal
  deriving Show

instance Eq Semantic where
  Semantic x == Semantic y =
    let (Decimal mx _, Decimal my _) = equalizeExponents x y
    in mx == my

instance Ord Semantic where
  compare (Semantic x) (Semantic y) =
    let (Decimal mx _, Decimal my _) = equalizeExponents x y
    in compare mx my

-- | Decimals whose significand is never zero.

data DecNonZero = DecNonZero !NonZero Unsigned
  deriving (Eq, Ord, Show)

class HasDecNonZero a where
  toDecNonZero :: a -> DecNonZero

instance HasOffset DecNonZero where
  offset (DecNonZero sig expt) = DecNonZero (offset sig) expt

decNonZeroToDecimal :: DecNonZero -> Decimal
decNonZeroToDecimal (DecNonZero nz u) = Decimal (nonZeroToInteger nz) u

decimalToDecNonZero :: Decimal -> Maybe DecNonZero
decimalToDecNonZero (Decimal signif expt) = case integerToNonZero signif of
  Nothing -> Nothing
  Just nz -> Just $ DecNonZero nz expt


-- | Decimals that are unsigned; they may be zero.
data DecUnsigned
  = DecUnsigned !Unsigned !Unsigned
  -- ^ @DecUnsigned a b@, where
  --
  -- @a@ is the significand, and
  --
  -- @b@ is the exponent
  deriving (Eq, Ord, Show)

class HasDecUnsigned a where
  toDecUnsigned :: a -> DecUnsigned

-- | Decimals that are positive; they may not be zero.
data DecPositive
  = DecPositive !Positive !Unsigned
  -- ^ @DecPositive a b@, where
  --
  -- @a@ is the significand, and
  --
  -- @b@ is the exponent
  deriving (Eq, Ord, Show)

class HasDecPositive a where
  toDecPositive :: a -> DecPositive

-- | Decimals whose significand is always zero.
data DecZero
  = DecZero !Unsigned
  -- ^ @DecZero a@, where @a@ is the exponent.  The significand is
  -- always zero so it does not have a field.
  deriving (Eq, Ord, Show)

class HasDecZero a where
  toDecZero :: a -> DecZero

instance HasDecimal DecZero where
  toDecimal (DecZero expt) = Decimal 0 expt

instance HasDecimal DecPositive where
  toDecimal (DecPositive sig expt) = Decimal (naturalToInteger sig) expt

instance HasExponent (NilUngrouped r) where
  toExponent nu = case nu of
    NUZero _ Nothing -> toUnsigned Zero
    NUZero _ (Just (_, Nothing)) -> toUnsigned Zero
    NUZero _ (Just (_, Just (_, zs))) -> next (lengthUnsigned zs)
    NURadix _ _ zs -> next (lengthUnsigned zs)

instance HasDecZero (NilUngrouped r) where
  toDecZero x = DecZero (toExponent x)

instance HasExponent (Nil r) where
  toExponent nil = case nil of
    NilU nu -> toExponent nu
    NilG ng -> toExponent ng

instance HasDecZero (Nil r) where
  toDecZero x = case x of
    NilU nu -> DecZero (toExponent nu)
    NilG ng -> DecZero (toExponent ng)

instance HasExponent (NilGrouped r) where
  toExponent (NilGrouped _ _ _ zs1 _ _ zs2 zss) =
      next . next . add (lengthUnsigned zs1) . add (lengthUnsigned zs2)
      . lengthUnsigned . join
      . fmap (\(_, _, sq) -> Zero <| sq) $ zss

instance HasDecZero (NilGrouped r) where
  toDecZero x = DecZero (toExponent x)

-- | Strips the sign from the 'DecNonZero'.
instance HasDecPositive DecNonZero where
  toDecPositive (DecNonZero sig expt) =
    DecPositive (nonZeroToPositive sig) expt

instance HasDecPositive (BrimUngrouped r) where

  toDecPositive (BUGreaterThanOne nv ds1 Nothing)
    = DecPositive (novDecsToPositive nv ds1) (toUnsigned Zero)

  toDecPositive (BUGreaterThanOne nv ds1 (Just (_, ds2)))
    = DecPositive (novDecsToPositive nv (ds1 <> ds2))
                  (lengthUnsigned ds2)

  toDecPositive (BULessThanOne _ _ zs1 nv ds)
    = DecPositive (novDecsToPositive nv ds)
                  (add (lengthUnsigned zs1) . next . lengthUnsigned $ ds)

instance HasDecPositive (BrimGrouped r) where
  toDecPositive = toDecPositive . ungroupBrimGrouped

instance HasDecPositive (Brim a) where
  toDecPositive (BrimGrouped a) = toDecPositive a
  toDecPositive (BrimUngrouped a) = toDecPositive a

instance HasDecPositive RepNonNeutralNoSide where
  toDecPositive (RepNonNeutralNoSide ei) =
    either toDecPositive toDecPositive ei

-- | Transforms a 'DecPositive' to a 'DecNonZero', while applying the
-- appropriate sign.
c'DecNonZero'DecPositive
  :: PluMin
  -> DecPositive
  -> DecNonZero
c'DecNonZero'DecPositive pm (DecPositive sig expt)
  = DecNonZero (c'NonZero'Positive pm sig) expt

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
stripDecimalSign (Decimal m e) = case stripIntegerSign m of
  Nothing -> Left (DecZero e)
  Just (p, pm) -> Right (DecPositive p e, pm)

stripNonZeroSign
  :: DecNonZero
  -> (DecPositive, PluMin)
stripNonZeroSign (DecNonZero nz ex)
  = (DecPositive (nonZeroToPositive nz) ex, nonZeroSign nz)

decomposeDecUnsigned
  :: DecUnsigned
  -> Either DecZero DecPositive
decomposeDecUnsigned (DecUnsigned m e) = case unsignedToPositive m of
  Nothing -> Left (DecZero e)
  Just m' -> Right (DecPositive m' e)

repUngroupedDecZero :: Radix r -> DecZero -> NilUngrouped r
repUngroupedDecZero rdx (DecZero expt) = case unsignedToPositive expt of
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
repUngroupedDecPositive rdx (DecPositive sig expt)
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


{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Decimal where
{-
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
  , repDecimal
  , displayDecimalAsQty
  ) where
-}

import Control.Lens (view, makeLenses, to, over, (<|), set)
import Control.Monad (join)
import Data.List (genericSplitAt, genericReplicate)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Sequence as S

import Penny.Digit
import Penny.Display
import Penny.Grammar
import Penny.Natural
import Penny.NonZero
import Penny.Polar

-- | Numbers represented exponentially.  In @Exponential c p@, the
-- value of the number is @c * 10 ^ (-1 * naturalToInteger p)@.
--
-- The 'Eq' and 'Ord' instances use 'equalizeExponents' first.
-- Therefore, for example, @3.5 == 3.500@ is 'True', even though the
-- values for both '_coefficient' and '_power' are different.
-- Similarly, @3.5 < 3.500@ is 'False', even though @3.5@ has a
-- '_coefficient' that is less than the '_coefficient' for @3.500@.
-- Usually this is what you want.  However, if it's not what you want,
-- just remove the '_coefficient' and the '_power' and put them into a
-- pair and then compare those using the derived instances of 'Eq' and
-- 'Ord'.
data Exponential c = Exponential
  { _coefficient :: !c
  -- ^ The significant digits; also known as the significand or the mantissa.
  , _power :: !Unsigned
  -- ^ The power of ten.
  } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Exponential

instance Polar c => Polar (Exponential c) where
  polar = view (coefficient . to polar)
  align pole = over coefficient (align pole)

instance Equatorial c => Equatorial (Exponential c) where
  equatorial = view (coefficient . to equatorial)

type Decimal = Exponential Integer

instance (Eq a, Pow a) => Eq (Exponential a) where
  x == y = view coefficient x' == view coefficient y'
    where
      (x', y') = equalizeExponents x y

instance (Ord a, Pow a) => Ord (Exponential a) where
  compare x y = comparing (view coefficient) x' y'
    where
      (x', y') = equalizeExponents x y

-- | Class for things that can be converted to a 'Decimal'.
class HasDecimal a where
  toDecimal :: a -> Decimal

instance HasDecimal Decimal where
  toDecimal = id

instance (HasDecimal a, HasDecimal b) => HasDecimal (Either a b) where
  toDecimal = either toDecimal toDecimal

instance (HasDecZero n, HasDecPositive o)
  => HasDecimal (Moderated n o) where
  toDecimal (Moderate n) = toDecimal . toDecZero $ n
  toDecimal (Extreme (Polarized o p))
    = changeSign (toDecimal . toDecPositive $ o)
    where
      changeSign
        | p == positive = id
        | otherwise = negate

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
  signum (Exponential mx _) = Exponential (signum mx) zero
  fromInteger i = Exponential i zero

-- | Decimals whose significand is never zero.
type DecNonZero = Exponential NonZero

class HasDecNonZero a where
  toDecNonZero :: a -> DecNonZero

decNonZeroToDecimal :: DecNonZero -> Decimal
decNonZeroToDecimal (Exponential nz u) = Exponential (nonZeroToInteger nz) u

decimalToDecNonZero :: Decimal -> Maybe DecNonZero
decimalToDecNonZero (Exponential signif expt) = case integerToNonZero signif of
  Nothing -> Nothing
  Just nz -> Just $ Exponential nz expt


-- | Decimals that are unsigned; they may be zero.
type DecUnsigned = Exponential Unsigned

class HasDecUnsigned a where
  toDecUnsigned :: a -> DecUnsigned

-- | Decimals that are positive; they may not be zero.
type DecPositive = Exponential Positive

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

instance HasExponent NilUngroupedRadCom where
  toExponent (NUZeroRadCom _ (RadixZeroesRadCom'Maybe Nothing)) = zero
  toExponent (NUZeroRadCom _ (RadixZeroesRadCom'Maybe
    (Just (RadixZeroesRadCom _ (Zero'Seq sq))))) = lengthUnsigned sq

instance HasExponent NilUngroupedRadPer where
  toExponent (NUZeroRadPer _ (RadixZeroesRadPer'Maybe Nothing)) = zero
  toExponent (NUZeroRadPer _ (RadixZeroesRadPer'Maybe
    (Just (RadixZeroesRadPer _ (Zero'Seq sq))))) = lengthUnsigned sq

instance HasDecZero NilUngroupedRadCom where
  toDecZero x = Exponential () (toExponent x)

instance HasDecZero NilUngroupedRadPer where
  toDecZero x = Exponential () (toExponent x)

instance HasExponent NilGroupedRadCom where
  toExponent (NilGroupedRadCom _zMay _rdx _z1 zs1 zss)
    = one `add` zeroes1 `add` zeroesRest
    where
      zeroes1 = let Zero'Seq zs = zs1 in lengthUnsigned zs
      zeroesRest = addGroup g1 (foldr addGroup zero gs)
        where
          ZeroGroupRadCom'Seq1 (g1, gs) = zss
          addGroup (ZeroGroupRadCom _ _zero1 (Zero'Seq zeros)) acc
            = one `add` lengthUnsigned zeros `add` acc

instance HasExponent NilGroupedRadPer where
  toExponent (NilGroupedRadPer _zMay _rdx _z1 zs1 zss)
    = one `add` zeroes1 `add` zeroesRest
    where
      zeroes1 = let Zero'Seq zs = zs1 in lengthUnsigned zs
      zeroesRest = addGroup g1 (foldr addGroup zero gs)
        where
          ZeroGroupRadPer'Seq1 (g1, gs) = zss
          addGroup (ZeroGroupRadPer _ _zero1 (Zero'Seq zeros)) acc
            = one `add` lengthUnsigned zeros `add` acc

instance HasDecZero NilGroupedRadCom where
  toDecZero x = Exponential () (toExponent x)

instance HasDecZero NilGroupedRadPer where
  toDecZero x = Exponential () (toExponent x)

instance HasExponent NilRadCom where
  toExponent (NilRadCom'NilUngroupedRadCom x) = toExponent x
  toExponent (NilRadCom'NilGroupedRadCom x) = toExponent x

instance HasExponent NilRadPer where
  toExponent (NilRadPer'NilUngroupedRadPer x) = toExponent x
  toExponent (NilRadPer'NilGroupedRadPer x) = toExponent x

instance HasDecZero NilRadCom where
  toDecZero (NilRadCom'NilUngroupedRadCom x)
    = Exponential () (toExponent x)
  toDecZero (NilRadCom'NilGroupedRadCom x)
    = Exponential () (toExponent x)

instance HasDecZero NilRadPer where
  toDecZero (NilRadPer'NilUngroupedRadPer x)
    = Exponential () (toExponent x)
  toDecZero (NilRadPer'NilGroupedRadPer x)
    = Exponential () (toExponent x)

-- | Strips the sign from the 'DecNonZero'.
instance HasDecPositive DecNonZero where
  toDecPositive (Exponential sig expt) =
    Exponential (nonZeroToPositive sig) expt

instance HasExponent RadixComDigits where
  toExponent (RadixComDigits _ (D0'9'Seq sq)) = lengthUnsigned sq

instance HasExponent RadixPerDigits where
  toExponent (RadixPerDigits _ (D0'9'Seq sq)) = lengthUnsigned sq

instance HasExponent RadComDigits'Maybe where
  toExponent (RadixComDigits'Maybe may)
    = maybe (const zero) toExponent may

instance HasExponent RadPerDigits'Maybe where
  toExponent (RadixPerDigits'Maybe may)
    = maybe (const zero) toExponent may

instance HasExponent BrimUngroupedRadCom where
  toExponent (BUGreaterThanOneRadCom _ _ mayRadCom) = toExponent mayRadCom
  toExponent (BULessThanOneRadCom _ _rdx zs1 _d2 (D0'9'Seq dss))
    = lengthUnsigned zs1 `add` one `add` lengthUnsigned dss

{-

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

instance (HasDecPositive a, HasDecPositive b)
  => HasDecPositive (Either a b) where
  toDecPositive = either toDecPositive toDecPositive

-- | Transforms a 'DecPositive' to a 'DecNonZero', while applying the
-- appropriate sign.
c'DecNonZero'DecPositive
  :: Pole
  -> DecPositive
  -> DecNonZero
c'DecNonZero'DecPositive pm (Exponential sig expt)
  = Exponential (align pm . c'NonZero'Positive $ sig) expt

-- * Representations

repUngroupedDecimal
  :: Radix r
  -> Decimal
  -> Moderated (NilUngrouped r) (BrimUngrouped r)
repUngroupedDecimal rdx d = case stripDecimalSign d of
  Left zero -> Moderate (repUngroupedDecZero rdx zero)
  Right (pos, pm) -> Extreme (Polarized (repUngroupedDecPositive rdx pos) pm)

repUngroupedDecNonZero
  :: Radix r
  -> DecNonZero
  -> (BrimUngrouped r, Pole)
repUngroupedDecNonZero rdx nz = (repUngroupedDecPositive rdx dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecUnsigned
  :: Radix r
  -> DecUnsigned
  -> Either (NilUngrouped r) (BrimUngrouped r)
repUngroupedDecUnsigned rdx uns = case decomposeDecUnsigned uns of
  Left z -> Left (repUngroupedDecZero rdx z)
  Right p -> Right (repUngroupedDecPositive rdx p)

-- Primitive grouping functions

stripDecimalSign
  :: Decimal
  -> Either DecZero (DecPositive, Pole)
stripDecimalSign (Exponential m e) = case stripIntegerSign m of
  Nothing -> Left (Exponential () e)
  Just (p, pm) -> Right (Exponential p e, pm)

stripNonZeroSign
  :: DecNonZero
  -> (DecPositive, Pole)
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


repDecimal
  :: Either (Maybe RadCom) (Maybe RadPer)
  -- ^ Determines which radix is used.  If you also supply a grouping
  -- character, 'repQty' will try to group the 'Qty' as well.
  -- Grouping will fail if the absolute value of the 'Qty' is less
  -- than @10000@.  In that case the 'Qty' will be represented without
  -- grouping.
  -> Decimal
  -> RepAnyRadix
repDecimal ei d = case ei of
  Left mayRadCom -> Left $ case mayRadCom of
    Nothing ->
      over _Moderate NilU
      . over _Extreme (fmap BrimUngrouped)
      . repUngroupedDecimal Radix
      $ d
    Just grpr -> case repUngroupedDecimal Radix d of
      Moderate nilUngr -> Moderate . NilU $ nilUngr
      Extreme plr -> case groupBrimUngrouped grpr (view charged plr) of
        Nothing -> Extreme . fmap BrimUngrouped $ plr
        Just bg -> Extreme (set charged (BrimGrouped bg) plr)

  Right mayRadPer -> Right $ case mayRadPer of
    Nothing ->
      over _Moderate NilU
      . over _Extreme (fmap BrimUngrouped)
      . repUngroupedDecimal Radix
      $ d
    Just grpr -> case repUngroupedDecimal Radix d of
      Moderate nilUngr -> Moderate . NilU $ nilUngr
      Extreme plr -> case groupBrimUngrouped grpr (view charged plr) of
        Nothing -> Extreme . fmap BrimUngrouped $ plr
        Just bg -> Extreme (set charged (BrimGrouped bg) plr)

-- | Provide a simple ungrouped string for a decimal.
displayDecimalAsQty
  :: Decimal
  -> ShowS
displayDecimalAsQty d = (sideChar :) .  (' ':) . rend
  where
    sideChar = case equatorial d of
      Nothing -> ' '
      Just v
        | v == debit -> '<'
        | otherwise -> '>'
    rend = case repUngroupedDecimal (Radix :: Radix RadPer) d of
      Moderate nu -> display nu
      Extreme (Polarized bu _) -> display bu
-}

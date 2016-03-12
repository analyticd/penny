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
  , repUngroupedDecimalRadCom
  , repUngroupedDecimalRadPer
  , repUngroupedDecNonZeroRadCom
  , repUngroupedDecNonZeroRadPer
  , repUngroupedDecUnsignedRadCom
  , repUngroupedDecUnsignedRadPer
  , repUngroupedDecZeroRadCom
  , repUngroupedDecZeroRadPer
  , repUngroupedDecPositiveRadCom
  , repUngroupedDecPositiveRadPer
  , repDigitsRadCom
  , repDigitsRadPer
  , repDecimal
  , displayDecimalAsQty
  ) where
-}

import Control.Lens (view, makeLenses, to, over, (<|))
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Prelude hiding (length)

import Penny.Digit
import Penny.Copper.Types
import Penny.Grouping
import Penny.NonNegative
import Penny.Natural
import Penny.NonZero
import Penny.Polar
import Penny.Positive (Positive)
import Penny.Rep

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
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''Exponential

type Decimal = Exponential Integer

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
  negate (Exponential mx ex) = Exponential (negate mx) ex
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

e'NilUngroupedRadCom :: NilUngroupedRadCom -> NonNegative
e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Maybe Nothing)) = zero
e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Maybe
  (Just (RadixZeroesRadCom _ (Zero'Seq sq))))) = length sq
e'NilUngroupedRadCom (NURadixRadCom _ _z1 (Zero'Seq zs))
  = one `add` (length zs)

e'NilUngroupedRadPer :: NilUngroupedRadPer -> NonNegative
e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Maybe Nothing)) = zero
e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Maybe
  (Just (RadixZeroesRadPer _ (Zero'Seq sq))))) = length sq
e'NilUngroupedRadPer (NURadixRadPer _ _z1 (Zero'Seq zs))
  = one `add` (length zs)

c'DecZero'NilUngroupedRadCom :: NilUngroupedRadCom -> DecZero
c'DecZero'NilUngroupedRadCom = Exponential () . e'NilUngroupedRadCom

c'DecZero'NilUngroupedRadPer :: NilUngroupedRadPer -> DecZero
c'DecZero'NilUngroupedRadPer = Exponential () . e'NilUngroupedRadPer

e'NilGroupedRadCom :: NilGroupedRadCom -> NonNegative
e'NilGroupedRadCom (NilGroupedRadCom _zMay _rdx _z1 zs1 zss)
  = one `add` zeroes1 `add` zeroesRest
  where
    zeroes1 = let Zero'Seq zs = zs1 in length zs
    zeroesRest = addGroup g1 (foldr addGroup zero gs)
      where
        ZeroGroupRadCom'Seq1 (g1, gs) = zss
        addGroup (ZeroGroupRadCom _ _zero1 (Zero'Seq zeros)) acc
          = one `add` length zeros `add` acc

e'NilGroupedRadPer :: NilGroupedRadPer -> NonNegative
e'NilGroupedRadPer (NilGroupedRadPer _zMay _rdx _z1 zs1 zss)
  = one `add` zeroes1 `add` zeroesRest
  where
    zeroes1 = let Zero'Seq zs = zs1 in length zs
    zeroesRest = addGroup g1 (foldr addGroup zero gs)
      where
        ZeroGroupRadPer'Seq1 (g1, gs) = zss
        addGroup (ZeroGroupRadPer _ _zero1 (Zero'Seq zeros)) acc
          = one `add` length zeros `add` acc

c'DecZero'NilGroupedRadCom :: NilGroupedRadCom -> DecZero
c'DecZero'NilGroupedRadCom = Exponential () . e'NilGroupedRadCom

c'DecZero'NilGroupedRadPer :: NilGroupedRadPer -> DecZero
c'DecZero'NilGroupedRadPer = Exponential () . e'NilGroupedRadPer

e'NilRadCom :: NilRadCom -> NonNegative
e'NilRadCom (NilRadCom'NilUngroupedRadCom x) = e'NilUngroupedRadCom x
e'NilRadCom (NilRadCom'NilGroupedRadCom x) = e'NilGroupedRadCom x

e'NilRadPer :: NilRadPer -> NonNegative
e'NilRadPer (NilRadPer'NilUngroupedRadPer x) = e'NilUngroupedRadPer x
e'NilRadPer (NilRadPer'NilGroupedRadPer x) = e'NilGroupedRadPer x

c'DecZero'NilRadCom :: NilRadCom -> DecZero
c'DecZero'NilRadCom = Exponential () . e'NilRadCom

c'DecZero'NilRadPer :: NilRadPer -> DecZero
c'DecZero'NilRadPer = Exponential () . e'NilRadPer

c'DecZero'Neutral :: Neutral -> DecZero
c'DecZero'Neutral (NeuCom _ n) = c'DecZero'NilRadCom n
c'DecZero'Neutral (NeuPer n) = c'DecZero'NilRadPer n

e'RadixComDigits :: RadixComDigits -> NonNegative
e'RadixComDigits (RadixComDigits _ (D0'9'Seq sq)) = length sq

e'RadixPerDigits :: RadixPerDigits -> NonNegative
e'RadixPerDigits (RadixPerDigits _ (D0'9'Seq sq)) = length sq

e'RadixComDigits'Maybe :: RadixComDigits'Maybe -> NonNegative
e'RadixComDigits'Maybe (RadixComDigits'Maybe may)
  = maybe zero e'RadixComDigits may

e'RadixPerDigits'Maybe :: RadixPerDigits'Maybe -> NonNegative
e'RadixPerDigits'Maybe (RadixPerDigits'Maybe may)
  = maybe zero e'RadixPerDigits may

e'BrimUngroupedRadCom :: BrimUngroupedRadCom -> NonNegative
e'BrimUngroupedRadCom
  (BUGreaterThanOneRadCom _ _ mayRadCom) = e'RadixComDigits'Maybe mayRadCom
e'BrimUngroupedRadCom
  (BULessThanOneRadCom _ _rdx (Zero'Seq zs1) _d2 (D0'9'Seq dss))
  = length zs1 `add` one `add` length dss

e'BrimUngroupedRadPer :: BrimUngroupedRadPer -> NonNegative
e'BrimUngroupedRadPer
  (BUGreaterThanOneRadPer _ _ mayRadPer) = e'RadixPerDigits'Maybe mayRadPer
e'BrimUngroupedRadPer
  (BULessThanOneRadPer _ _rdx (Zero'Seq zs1) _d2 (D0'9'Seq dss))
  = length zs1 `add` one `add` length dss

{-

instance HasDecPositive BrimUngroupedRadCom where
  toDecPositive (BUGreaterThanOneRadCom nv (D0'9'Seq ds1)
    (RadixComDigits'Maybe Nothing))
    = Exponential (novDecsToPositive nv ds1) zero

  toDecPositive (BUGreaterThanOneRadCom nv (D0'9'Seq ds1)
    (RadixComDigits'Maybe (Just (RadixComDigits _ (D0'9'Seq ds2)))))
    = Exponential (novDecsToPositive nv (ds1 <> ds2))
                  (lengthUnsigned ds2)

  toDecPositive (BULessThanOneRadCom _ _ (Zero'Seq zs1) nv (D0'9'Seq ds))
    = Exponential (novDecsToPositive nv ds)
                  (one `add` (lengthUnsigned zs1) `add` (lengthUnsigned ds))

instance HasDecPositive BrimUngroupedRadPer where
  toDecPositive (BUGreaterThanOneRadPer nv (D0'9'Seq ds1)
    (RadixPerDigits'Maybe Nothing))
    = Exponential (novDecsToPositive nv ds1) zero

  toDecPositive (BUGreaterThanOneRadPer nv (D0'9'Seq ds1)
    (RadixPerDigits'Maybe (Just (RadixPerDigits _ (D0'9'Seq ds2)))))
    = Exponential (novDecsToPositive nv (ds1 <> ds2))
                  (lengthUnsigned ds2)

  toDecPositive (BULessThanOneRadPer _ _ (Zero'Seq zs1) nv (D0'9'Seq ds))
    = Exponential (novDecsToPositive nv ds)
                  (one `add` (lengthUnsigned zs1) `add` (lengthUnsigned ds))

instance HasDecPositive BrimGroupedRadCom where
  toDecPositive = toDecPositive . ungroupBrimGroupedRadCom

instance HasDecPositive BrimGroupedRadPer where
  toDecPositive = toDecPositive . ungroupBrimGroupedRadPer

instance HasDecPositive BrimRadCom where
  toDecPositive = toDecPositive . ungroupBrimRadCom

instance HasDecPositive BrimRadPer where
  toDecPositive = toDecPositive . ungroupBrimRadPer

instance HasDecPositive NonNeutral where
  toDecPositive (NonNeutralRadCom _ b) = toDecPositive b
  toDecPositive (NonNeutralRadPer b) = toDecPositive b

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

repDigitsRadCom
  :: (D1'9, Seq D0'9)
  -- ^ Significand
  -> Unsigned
  -- ^ Exponent
  -> BrimUngroupedRadCom
repDigitsRadCom (d1, dr) expt
  = case diffUnsigned (next $ lengthUnsigned dr) expt of
      Equal -> BULessThanOneRadCom (Zero'Maybe $ Just zero)
        rdx (Zero'Seq S.empty) d1 (D0'9'Seq dr)
      LeftBiggerBy l -> BUGreaterThanOneRadCom d1 (D0'9'Seq leftDigs)
        (RadixComDigits'Maybe rightDigs)
        where
          (leftDigs, rightDigs) = case prev l of
            Nothing -> (S.empty, Just (RadixComDigits rdx (D0'9'Seq dr)))
            Just c -> (beg,
              Just (RadixComDigits rdx (D0'9'Seq end)))
              where
                (beg, end) = S.splitAt (integerToInt $ naturalToInteger c) dr
      RightBiggerBy r -> BULessThanOneRadCom (Zero'Maybe $ Just zero)
        rdx (Zero'Seq zs) d1 (D0'9'Seq dr)
        where
          zs = flip S.replicate zero
            . integerToInt . naturalToInteger $ r
  where
    rdx = RadixCom ','

repDigitsRadPer
  :: (D1'9, Seq D0'9)
  -- ^ Significand
  -> Unsigned
  -- ^ Exponent
  -> BrimUngroupedRadPer
repDigitsRadPer (d1, dr) expt
  = case diffUnsigned (next $ lengthUnsigned dr) expt of
      Equal -> BULessThanOneRadPer (Zero'Maybe $ Just zero)
        rdx (Zero'Seq S.empty) d1 (D0'9'Seq dr)
      LeftBiggerBy l -> BUGreaterThanOneRadPer d1 (D0'9'Seq leftDigs)
        (RadixPerDigits'Maybe rightDigs)
        where
          (leftDigs, rightDigs) = case prev l of
            Nothing -> (S.empty, Just (RadixPerDigits rdx (D0'9'Seq dr)))
            Just c -> (beg,
              Just (RadixPerDigits rdx (D0'9'Seq end)))
              where
                (beg, end) = S.splitAt (integerToInt $ naturalToInteger c) dr
      RightBiggerBy r -> BULessThanOneRadPer (Zero'Maybe $ Just zero)
        rdx (Zero'Seq zs) d1 (D0'9'Seq dr)
        where
          zs = flip S.replicate zero
            . integerToInt . naturalToInteger $ r
  where
    rdx = RadixPer '.'

integerToInt :: Integer -> Int
integerToInt x
  | x < fromIntegral (minBound :: Int) = error "integer too small"
  | x > fromIntegral (maxBound :: Int) = error "integer too large"
  | otherwise = fromIntegral x


repUngroupedDecZeroRadCom
  :: DecZero
  -> NilUngroupedRadCom
repUngroupedDecZeroRadCom (Exponential () expt) = NUZeroRadCom zero
  (RadixZeroesRadCom'Maybe mayRdx)
  where
    rdx = RadixCom ','
    mayRdx
      | expt == zero = Nothing
      | otherwise = Just (RadixZeroesRadCom rdx (Zero'Seq zs))
      where
        zs = S.replicate (integerToInt . naturalToInteger $ expt) zero

repUngroupedDecZeroRadPer
  :: DecZero
  -> NilUngroupedRadPer
repUngroupedDecZeroRadPer (Exponential () expt) = NUZeroRadPer zero
  (RadixZeroesRadPer'Maybe mayRdx)
  where
    rdx = RadixPer '.'
    mayRdx
      | expt == zero = Nothing
      | otherwise = Just (RadixZeroesRadPer rdx (Zero'Seq zs))
      where
        zs = S.replicate (integerToInt . naturalToInteger $ expt) zero

repUngroupedDecPositiveRadCom
  :: DecPositive
  -> BrimUngroupedRadCom
repUngroupedDecPositiveRadCom (Exponential sig expt)
  = repDigitsRadCom (positiveDigits sig) expt

repUngroupedDecPositiveRadPer
  :: DecPositive
  -> BrimUngroupedRadPer
repUngroupedDecPositiveRadPer (Exponential sig expt)
  = repDigitsRadPer (positiveDigits sig) expt

decomposeDecUnsigned
  :: DecUnsigned
  -> Either DecZero DecPositive
decomposeDecUnsigned (Exponential m e) = case unsignedToPositive m of
  Nothing -> Left (Exponential () e)
  Just m' -> Right (Exponential m' e)

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

repUngroupedDecNonZeroRadCom
  :: DecNonZero
  -> (BrimUngroupedRadCom, Pole)
repUngroupedDecNonZeroRadCom nz = (repUngroupedDecPositiveRadCom dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecNonZeroRadPer
  :: DecNonZero
  -> (BrimUngroupedRadPer, Pole)
repUngroupedDecNonZeroRadPer nz = (repUngroupedDecPositiveRadPer dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecimalRadCom
  :: Decimal
  -> Moderated NilUngroupedRadCom BrimUngroupedRadCom
repUngroupedDecimalRadCom d = case stripDecimalSign d of
  Left zero -> Moderate (repUngroupedDecZeroRadCom zero)
  Right (pos, pm) ->
    Extreme (Polarized (repUngroupedDecPositiveRadCom pos) pm)

repUngroupedDecimalRadPer
  :: Decimal
  -> Moderated NilUngroupedRadPer BrimUngroupedRadPer
repUngroupedDecimalRadPer d = case stripDecimalSign d of
  Left zero -> Moderate (repUngroupedDecZeroRadPer zero)
  Right (pos, pm) ->
    Extreme (Polarized (repUngroupedDecPositiveRadPer pos) pm)

repUngroupedDecUnsignedRadCom
  :: DecUnsigned
  -> Either NilUngroupedRadCom BrimUngroupedRadCom
repUngroupedDecUnsignedRadCom uns = case decomposeDecUnsigned uns of
  Left z -> Left (repUngroupedDecZeroRadCom z)
  Right p -> Right (repUngroupedDecPositiveRadCom p)

repUngroupedDecUnsignedRadPer
  :: DecUnsigned
  -> Either NilUngroupedRadPer BrimUngroupedRadPer
repUngroupedDecUnsignedRadPer uns = case decomposeDecUnsigned uns of
  Left z -> Left (repUngroupedDecZeroRadPer z)
  Right p -> Right (repUngroupedDecPositiveRadPer p)

repDecimal
  :: Either (Maybe GrpRadCom) (Maybe GrpRadPer)
  -- ^ Determines which radix is used.  If you also supply a grouping
  -- character, 'repDecimal' will try to group the 'Decimal' as well.
  -- Grouping will fail if the absolute value of the 'Decimal' is less
  -- than @1000@.  In that case the 'Decimal' will be represented without
  -- grouping.
  -> Decimal
  -> RepAnyRadix
repDecimal ei d = case ei of
  Left mayRadCom -> Left $ case mayRadCom of
    Nothing -> case repUngroupedDecimalRadCom d of
      Moderate nilU -> Moderate $ NilRadCom'NilUngroupedRadCom nilU
      Extreme (Polarized brimU side) -> Extreme
        (Polarized (BrimRadCom'BrimUngroupedRadCom brimU) side)
    Just grpr -> case repUngroupedDecimalRadCom d of
      Moderate nilU -> Moderate $ NilRadCom'NilUngroupedRadCom nilU
      Extreme (Polarized brimU side) ->
        case groupBrimUngroupedRadCom grpr brimU of
          Nothing -> Extreme
            (Polarized (BrimRadCom'BrimUngroupedRadCom brimU) side)
          Just grouped -> Extreme
            (Polarized (BrimRadCom'BrimGroupedRadCom grouped) side)

  Right mayRadPer -> Right $ case mayRadPer of
    Nothing -> case repUngroupedDecimalRadPer d of
      Moderate nilU -> Moderate $ NilRadPer'NilUngroupedRadPer nilU
      Extreme (Polarized brimU side) -> Extreme
        (Polarized (BrimRadPer'BrimUngroupedRadPer brimU) side)
    Just grpr -> case repUngroupedDecimalRadPer d of
      Moderate nilU -> Moderate $ NilRadPer'NilUngroupedRadPer nilU
      Extreme (Polarized brimU side) ->
        case groupBrimUngroupedRadPer grpr brimU of
          Nothing -> Extreme
            (Polarized (BrimRadPer'BrimUngroupedRadPer brimU) side)
          Just grouped -> Extreme
            (Polarized (BrimRadPer'BrimGroupedRadPer grouped) side)

-- | Provide a simple ungrouped string for a decimal.
displayDecimalAsQty
  :: Decimal
  -> ShowS
displayDecimalAsQty d = (toList (sideChar <| ' ' <| rest) ++)
  where
    sideChar = case equatorial d of
      Nothing -> ' '
      Just v
        | v == debit -> '<'
        | otherwise -> '>'
    rest = case repUngroupedDecimalRadPer d of
      Moderate nu -> t'NilUngroupedRadPer nu
      Extreme (Polarized bu _) -> t'BrimUngroupedRadPer bu
-}

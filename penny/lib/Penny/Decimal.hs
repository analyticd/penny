{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Decimal where

import Control.Lens (makeLenses, (<|))
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Prelude hiding (length)

import qualified Penny.Copper.Conversions as Conv
import Penny.Copper.Singleton
import Penny.Copper.Terminalizers
import Penny.Copper.Types
import Penny.Grouping
import Penny.NonNegative
import Penny.NonZero
import qualified Penny.NonZero as NonZero
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos
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


repDigitsRadCom
  :: (D1'9 Char (), Seq (D0'9 Char ()))
  -- ^ Significand
  -> NonNegative
  -- ^ Exponent
  -> BrimUngroupedRadCom Char ()
repDigitsRadCom (d1, dr) expt
  = case diff (next $ length dr) expt of
      Equal -> BULessThanOneRadCom (Zero'Opt $ Just sZero)
        rdx (Zero'Star S.empty) d1 (D0'9'Star dr)
      LeftBiggerBy l -> BUGreaterThanOneRadCom d1 (D0'9'Star leftDigs)
        (RadixComDigits'Opt rightDigs)
        where
          (leftDigs, rightDigs) = case Pos.prev l of
            Nothing -> (S.empty, Just (RadixComDigits rdx (D0'9'Star dr)))
            Just c -> (beg,
              Just (RadixComDigits rdx (D0'9'Star end)))
              where
                (beg, end) = S.splitAt (integerToInt $ Pos.c'Integer'Positive c) dr
      RightBiggerBy r -> BULessThanOneRadCom (Zero'Opt $ Just sZero)
        rdx (Zero'Star zs) d1 (D0'9'Star dr)
        where
          zs = flip S.replicate sZero
            . integerToInt . Pos.c'Integer'Positive $ r
  where
    rdx = sRadixCom

repDigitsRadPer
  :: (D1'9 Char (), Seq (D0'9 Char ()))
  -- ^ Significand
  -> NonNegative
  -- ^ Exponent
  -> BrimUngroupedRadPer Char () 
repDigitsRadPer (d1, dr) expt
  = case diff (next $ length dr) expt of
      Equal -> BULessThanOneRadPer (Zero'Opt $ Just sZero)
        rdx (Zero'Star S.empty) d1 (D0'9'Star dr)
      LeftBiggerBy l -> BUGreaterThanOneRadPer d1 (D0'9'Star leftDigs)
        (RadixPerDigits'Opt rightDigs)
        where
          (leftDigs, rightDigs) = case Pos.prev l of
            Nothing -> (S.empty, Just (RadixPerDigits rdx (D0'9'Star dr)))
            Just c -> (beg,
              Just (RadixPerDigits rdx (D0'9'Star end)))
              where
                (beg, end) = S.splitAt (integerToInt $ Pos.c'Integer'Positive c) dr
      RightBiggerBy r -> BULessThanOneRadPer (Zero'Opt $ Just sZero)
        rdx (Zero'Star zs) d1 (D0'9'Star dr)
        where
          zs = flip S.replicate sZero
            . integerToInt . Pos.c'Integer'Positive $ r
  where
    rdx = sRadixPer

repUngroupedDecZeroRadCom
  :: DecZero
  -> NilUngroupedRadCom Char ()
repUngroupedDecZeroRadCom (Exponential () expt) = NUZeroRadCom sZero
  (RadixZeroesRadCom'Opt mayRdx)
  where
    rdx = sRadixCom
    mayRdx
      | expt == zero = Nothing
      | otherwise = Just (RadixZeroesRadCom rdx (Zero'Star zs))
      where
        zs = S.replicate (integerToInt . c'Integer'NonNegative $ expt) sZero

repUngroupedDecZeroRadPer
  :: DecZero
  -> NilUngroupedRadPer Char ()
repUngroupedDecZeroRadPer (Exponential () expt) = NUZeroRadPer sZero
  (RadixZeroesRadPer'Opt mayRdx)
  where
    rdx = sRadixPer
    mayRdx
      | expt == zero = Nothing
      | otherwise = Just (RadixZeroesRadPer rdx (Zero'Star zs))
      where
        zs = S.replicate (integerToInt . c'Integer'NonNegative $ expt) sZero

repUngroupedDecPositiveRadCom
  :: DecPositive
  -> BrimUngroupedRadCom Char ()
repUngroupedDecPositiveRadCom (Exponential sig expt)
  = repDigitsRadCom (Conv.positiveDigits sig) expt

repUngroupedDecPositiveRadPer
  :: DecPositive
  -> BrimUngroupedRadPer Char ()
repUngroupedDecPositiveRadPer (Exponential sig expt)
  = repDigitsRadPer (Conv.positiveDigits sig) expt

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

repUngroupedDecNonZeroRadCom
  :: DecNonZero
  -> (BrimUngroupedRadCom Char (), Pole)
repUngroupedDecNonZeroRadCom nz = (repUngroupedDecPositiveRadCom dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecNonZeroRadPer
  :: DecNonZero
  -> (BrimUngroupedRadPer Char (), Pole)
repUngroupedDecNonZeroRadPer nz = (repUngroupedDecPositiveRadPer dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecimalRadCom
  :: Decimal
  -> Moderated (NilUngroupedRadCom Char ()) (BrimUngroupedRadCom Char ())
repUngroupedDecimalRadCom d = case stripDecimalSign d of
  Left zero -> Moderate (repUngroupedDecZeroRadCom zero)
  Right (pos, pm) ->
    Extreme (Polarized (repUngroupedDecPositiveRadCom pos) pm)

repUngroupedDecimalRadPer
  :: Decimal
  -> Moderated (NilUngroupedRadPer Char ()) (BrimUngroupedRadPer Char ())
repUngroupedDecimalRadPer d = case stripDecimalSign d of
  Left zero -> Moderate (repUngroupedDecZeroRadPer zero)
  Right (pos, pm) ->
    Extreme (Polarized (repUngroupedDecPositiveRadPer pos) pm)

repUngroupedDecUnsignedRadCom
  :: DecUnsigned
  -> Either (NilUngroupedRadCom Char ()) (BrimUngroupedRadCom Char ())
repUngroupedDecUnsignedRadCom uns = case decomposeDecUnsigned uns of
  Left z -> Left (repUngroupedDecZeroRadCom z)
  Right p -> Right (repUngroupedDecPositiveRadCom p)

repUngroupedDecUnsignedRadPer
  :: DecUnsigned
  -> Either (NilUngroupedRadPer Char ()) (BrimUngroupedRadPer Char ())
repUngroupedDecUnsignedRadPer uns = case decomposeDecUnsigned uns of
  Left z -> Left (repUngroupedDecZeroRadPer z)
  Right p -> Right (repUngroupedDecPositiveRadPer p)

repDecimal
  :: Either (Maybe (GrpRadCom Char ())) (Maybe (GrpRadPer Char ()))
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
    sideChar = case integerPole . _coefficient $ d of
      Nothing -> ' '
      Just v
        | v == debit -> '<'
        | otherwise -> '>'
    rest = fmap fst . toList $ case repUngroupedDecimalRadPer d of
      Moderate nu -> t'NilUngroupedRadPer nu
      Extreme (Polarized bu _) -> t'BrimUngroupedRadPer bu



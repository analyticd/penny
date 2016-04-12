{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Decimal where

import Control.Lens (makeLenses, (<|))
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Prelude hiding (length)
import qualified Pinchot

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

e'NilUngroupedRadCom :: NilUngroupedRadCom t a -> NonNegative
e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Opt Nothing)) = zero
e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Opt
  (Just (RadixZeroesRadCom _ (Zero'Star sq))))) = length sq
e'NilUngroupedRadCom (NURadixRadCom _ _z1 (Zero'Star zs))
  = one `add` (length zs)

e'NilUngroupedRadPer :: NilUngroupedRadPer t a -> NonNegative
e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Opt Nothing)) = zero
e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Opt
  (Just (RadixZeroesRadPer _ (Zero'Star sq))))) = length sq
e'NilUngroupedRadPer (NURadixRadPer _ _z1 (Zero'Star zs))
  = one `add` (length zs)

c'DecZero'NilUngroupedRadCom :: NilUngroupedRadCom t a -> DecZero
c'DecZero'NilUngroupedRadCom = Exponential () . e'NilUngroupedRadCom

c'DecZero'NilUngroupedRadPer :: NilUngroupedRadPer t a -> DecZero
c'DecZero'NilUngroupedRadPer = Exponential () . e'NilUngroupedRadPer

e'NilGroupedRadCom :: NilGroupedRadCom t a -> NonNegative
e'NilGroupedRadCom (NilGroupedRadCom _zMay _rdx _z1 zs1 zss)
  = one `add` zeroes1 `add` zeroesRest
  where
    zeroes1 = let Zero'Star zs = zs1 in length zs
    zeroesRest = addGroup g1 (foldr addGroup zero gs)
      where
        ZeroGroupRadCom'Plus (Pinchot.NonEmpty g1 gs) = zss
        addGroup (ZeroGroupRadCom _ _zero1 (Zero'Star zeros)) acc
          = one `add` length zeros `add` acc

e'NilGroupedRadPer :: NilGroupedRadPer t a -> NonNegative
e'NilGroupedRadPer (NilGroupedRadPer _zMay _rdx _z1 zs1 zss)
  = one `add` zeroes1 `add` zeroesRest
  where
    zeroes1 = let Zero'Star zs = zs1 in length zs
    zeroesRest = addGroup g1 (foldr addGroup zero gs)
      where
        ZeroGroupRadPer'Plus (Pinchot.NonEmpty g1 gs) = zss
        addGroup (ZeroGroupRadPer _ _zero1 (Zero'Star zeros)) acc
          = one `add` length zeros `add` acc

c'DecZero'NilGroupedRadCom :: NilGroupedRadCom t a -> DecZero
c'DecZero'NilGroupedRadCom = Exponential () . e'NilGroupedRadCom

c'DecZero'NilGroupedRadPer :: NilGroupedRadPer t a -> DecZero
c'DecZero'NilGroupedRadPer = Exponential () . e'NilGroupedRadPer

e'NilRadCom :: NilRadCom t a -> NonNegative
e'NilRadCom (NilRadCom'NilUngroupedRadCom x) = e'NilUngroupedRadCom x
e'NilRadCom (NilRadCom'NilGroupedRadCom x) = e'NilGroupedRadCom x

e'NilRadPer :: NilRadPer t a -> NonNegative
e'NilRadPer (NilRadPer'NilUngroupedRadPer x) = e'NilUngroupedRadPer x
e'NilRadPer (NilRadPer'NilGroupedRadPer x) = e'NilGroupedRadPer x

c'DecZero'NilRadCom :: NilRadCom t a -> DecZero
c'DecZero'NilRadCom = Exponential () . e'NilRadCom

c'DecZero'NilRadPer :: NilRadPer t a -> DecZero
c'DecZero'NilRadPer = Exponential () . e'NilRadPer

c'DecZero'Neutral :: Neutral t a -> DecZero
c'DecZero'Neutral (NeuCom _ n) = c'DecZero'NilRadCom n
c'DecZero'Neutral (NeuPer n) = c'DecZero'NilRadPer n

e'RadixComDigits :: RadixComDigits t a -> NonNegative
e'RadixComDigits (RadixComDigits _ (D0'9'Star sq)) = length sq

e'RadixPerDigits :: RadixPerDigits t a -> NonNegative
e'RadixPerDigits (RadixPerDigits _ (D0'9'Star sq)) = length sq

e'RadixComDigits'Opt :: RadixComDigits'Opt t a -> NonNegative
e'RadixComDigits'Opt (RadixComDigits'Opt may)
  = maybe zero e'RadixComDigits may

e'RadixPerDigits'Opt :: RadixPerDigits'Opt t a -> NonNegative
e'RadixPerDigits'Opt (RadixPerDigits'Opt may)
  = maybe zero e'RadixPerDigits may

e'BrimUngroupedRadCom :: BrimUngroupedRadCom t a -> NonNegative
e'BrimUngroupedRadCom
  (BUGreaterThanOneRadCom _ _ mayRadCom) = e'RadixComDigits'Opt mayRadCom
e'BrimUngroupedRadCom
  (BULessThanOneRadCom _ _rdx (Zero'Star zs1) _d2 (D0'9'Star dss))
  = length zs1 `add` one `add` length dss

e'BrimUngroupedRadPer :: BrimUngroupedRadPer t a -> NonNegative
e'BrimUngroupedRadPer
  (BUGreaterThanOneRadPer _ _ mayRadPer) = e'RadixPerDigits'Opt mayRadPer
e'BrimUngroupedRadPer
  (BULessThanOneRadPer _ _rdx (Zero'Star zs1) _d2 (D0'9'Star dss))
  = length zs1 `add` one `add` length dss

c'DecPositive'BrimUngroupedRadCom :: BrimUngroupedRadCom t a -> DecPositive
c'DecPositive'BrimUngroupedRadCom (BUGreaterThanOneRadCom nv (D0'9'Star ds1)
  (RadixComDigits'Opt Nothing))
  = Exponential (Conv.novDecsToPositive nv ds1) zero

c'DecPositive'BrimUngroupedRadCom (BUGreaterThanOneRadCom nv (D0'9'Star ds1)
  (RadixComDigits'Opt (Just (RadixComDigits _ (D0'9'Star ds2)))))
  = Exponential (Conv.novDecsToPositive nv (ds1 <> ds2))
                (length ds2)

c'DecPositive'BrimUngroupedRadCom
  (BULessThanOneRadCom _ _ (Zero'Star zs1) nv (D0'9'Star ds))
  = Exponential (Conv.novDecsToPositive nv ds)
                (one `add` (length zs1) `add` (length ds))

c'DecPositive'BrimUngroupedRadPer :: BrimUngroupedRadPer t a -> DecPositive
c'DecPositive'BrimUngroupedRadPer (BUGreaterThanOneRadPer nv (D0'9'Star ds1)
  (RadixPerDigits'Opt Nothing))
  = Exponential (Conv.novDecsToPositive nv ds1) zero

c'DecPositive'BrimUngroupedRadPer (BUGreaterThanOneRadPer nv (D0'9'Star ds1)
  (RadixPerDigits'Opt (Just (RadixPerDigits _ (D0'9'Star ds2)))))
  = Exponential (Conv.novDecsToPositive nv (ds1 <> ds2))
                (length ds2)

c'DecPositive'BrimUngroupedRadPer
  (BULessThanOneRadPer _ _ (Zero'Star zs1) nv (D0'9'Star ds))
  = Exponential (Conv.novDecsToPositive nv ds)
                (one `add` (length zs1) `add` (length ds))

c'DecPositive'BrimGroupedRadCom :: BrimGroupedRadCom t a -> DecPositive
c'DecPositive'BrimGroupedRadCom
  = c'DecPositive'BrimUngroupedRadCom . ungroupBrimGroupedRadCom

c'DecPositive'BrimGroupedRadPer :: BrimGroupedRadPer t a -> DecPositive
c'DecPositive'BrimGroupedRadPer
  = c'DecPositive'BrimUngroupedRadPer . ungroupBrimGroupedRadPer

c'DecPositive'BrimRadCom :: BrimRadCom t a -> DecPositive
c'DecPositive'BrimRadCom
  = c'DecPositive'BrimUngroupedRadCom . ungroupBrimRadCom

c'DecPositive'BrimRadPer :: BrimRadPer t a -> DecPositive
c'DecPositive'BrimRadPer
  = c'DecPositive'BrimUngroupedRadPer . ungroupBrimRadPer

c'DecPositive'NonNeutral :: NonNeutral t a -> DecPositive
c'DecPositive'NonNeutral (NonNeutralRadCom _ b) = c'DecPositive'BrimRadCom b
c'DecPositive'NonNeutral (NonNeutralRadPer b) = c'DecPositive'BrimRadPer b

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

c'Decimal'RepRadCom :: RepRadCom -> Decimal
c'Decimal'RepRadCom x = case x of
  Moderate nrc -> Lens.over coefficient (const 0)
    . c'DecZero'NilRadCom $ nrc
  Extreme (Polarized brimRadCom pole) ->
      Lens.over coefficient (changeSign . Pos.c'Integer'Positive)
      . c'DecPositive'BrimRadCom
      $ brimRadCom
      where
        changeSign
          | pole == positive = id
          | otherwise = Prelude.negate

c'Decimal'RepRadPer :: RepRadPer -> Decimal
c'Decimal'RepRadPer x = case x of
  Moderate nrc -> Lens.over coefficient (const 0)
    . c'DecZero'NilRadPer $ nrc
  Extreme (Polarized brimRadPer pole) ->
      Lens.over coefficient (changeSign . Pos.c'Integer'Positive)
      . c'DecPositive'BrimRadPer
      $ brimRadPer
      where
        changeSign
          | pole == positive = id
          | otherwise = Prelude.negate

c'Decimal'RepAnyRadix :: RepAnyRadix -> Decimal
c'Decimal'RepAnyRadix = either c'Decimal'RepRadCom c'Decimal'RepRadPer

c'DecPositive'BrimAnyRadix :: BrimAnyRadix -> DecPositive
c'DecPositive'BrimAnyRadix
  = either c'DecPositive'BrimRadCom c'DecPositive'BrimRadPer

c'DecZero'NilAnyRadix :: NilAnyRadix -> DecZero
c'DecZero'NilAnyRadix = either c'DecZero'NilRadCom c'DecZero'NilRadPer

c'Decimal'DecNonZero :: DecNonZero -> Decimal
c'Decimal'DecNonZero = fmap c'Integer'NonZero

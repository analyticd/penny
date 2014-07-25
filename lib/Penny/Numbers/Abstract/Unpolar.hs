{-# LANGUAGE BangPatterns #-}
-- | Unpolar abstract numbers.
module Penny.Numbers.Abstract.Unpolar where

import Data.Sequence (Seq)
import Penny.Numbers.Abstract.RadGroup
import Deka.Native.Abstract
import Data.Maybe

data NonNegative = Zero | Plus !Positive
  deriving (Eq, Ord, Show)

intToNonNegative :: Integral a => a -> Maybe NonNegative
intToNonNegative a
  | a < 0 = Nothing
  | a == 0 = Just Zero
  | otherwise = Just . Plus
      . fromMaybe (error "intToNonNegative: error") . intToPositive $ a

data Positive = One | Succ !Positive
  deriving (Eq, Ord, Show)

intToPositive :: Integral a => a -> Maybe Positive
intToPositive a
  | a < 1 = Nothing
  | otherwise = Just $ go One a
  where
    go soFar i
      | i == 1 = soFar
      | otherwise = go (Succ soFar) (pred i)

novemToPositive :: Novem -> Positive
novemToPositive n = case n of
  D1 -> One
  D2 -> Succ One
  D3 -> Succ . Succ $ One
  D4 -> Succ . Succ . Succ $ One
  D5 -> Succ . Succ . Succ . Succ $ One
  D6 -> Succ . Succ . Succ . Succ . Succ $ One
  D7 -> Succ . Succ . Succ . Succ . Succ . Succ $ One
  D8 -> Succ . Succ . Succ . Succ . Succ . Succ . Succ $ One
  D9 -> Succ . Succ . Succ . Succ . Succ . Succ . Succ . Succ $ One

decemToNonNegative :: Decem -> NonNegative
decemToNonNegative d = case d of
  D0 -> Zero
  Nonem n -> Plus . novemToPositive $ n

-- | Exponents.  Unlike exponents in Deka, Penny does not use
-- positive exponents because there is no unambiguous way to
-- represent them using ordinary notation.  All exponents are either
-- negative or zero.

data Exponent
  = ExpZero
  | ExpNegative NovDecs
  deriving (Eq, Ord, Show)

data NovDecs = NovDecs
  { ndNovem :: Novem
  , ndDecems :: Seq Decem
  } deriving (Eq, Ord, Show)

novDecsToPositive :: NovDecs -> Positive
novDecsToPositive (NovDecs n ds) = finish $ go 0 Zero ds
  where
    (finish, go) = undefined

loop :: Integer -> (a -> a) -> a -> a
loop !i f a
  | i <= 0 = a
  | otherwise = loop (i - 1) f (f a)

-- | Coefficients.  Different from Deka coefficients in form but not
-- substance.

data Coefficient
  = CoeZero
  | CoeNonZero NovDecs
  deriving (Eq, Ord, Show)

data ZeroesNovDecs = ZeroesNovDecs
  { zndZeroes :: NonNegative
  , zndNovDecs :: NovDecs
  } deriving (Eq, Ord, Show)

data DecDecs = DecDecs Decem (Seq Decem)
  deriving (Eq, Ord, Show)

newtype HasZeroDigit = HasZeroDigit { unHasZeroDigit :: Bool }
  deriving (Eq, Ord, Show)

data ZeroDigit = ZeroDigit
  deriving (Eq, Ord, Show)

data Zeroes = Zeroes Positive
  deriving (Eq, Ord, Show)

-- Ungrouped - non-zero

data UNWhole = UNWhole NovDecs
  deriving (Eq, Ord, Show)

data UNWholeRadix r = UNWholeRadix NovDecs (Radix r) (Maybe DecDecs)
  deriving (Eq, Ord, Show)

data UNRadFrac r = UNRadFrac (Maybe ZeroDigit) (Radix r) ZeroesNovDecs
  deriving (Eq, Ord, Show)

-- Ungrouped - zero

data UZBare = UZBare ZeroDigit
  deriving (Eq, Ord, Show)

data UZTrailing r = UZTrailing ZeroDigit (Radix r) (Maybe Zeroes)
  deriving (Eq, Ord, Show)

-- Grouped - zero

data GZ r = GZ (Maybe ZeroDigit) (Radix r) Zeroes (Group r Zeroes)
                 (Seq (Group r Zeroes))
  deriving (Eq, Ord, Show)

-- Grouped - non-zero

-- Grouped - greater than or equal to one

-- | Greater than or equal to one, grouped on left side.  No radix.
data MasunoGroupedLeft r =
  MasunoGroupedLeft NovDecs (Group r DecDecs) (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)

-- | Greater than or equal to one, grouped on left side, with radix.
-- Optional grouping on right side.
data MasunoGroupedLeftRad r =
  MasunoGroupedLeftRad (MasunoGroupedLeft r)
                       (Radix r)
                       (Maybe (DecDecs, Seq (Group r DecDecs)))
  deriving (Eq, Ord, Show)

-- | Greater than or equal to one, grouped on right side only.

data MasunoGroupedRight r =
  MasunoGroupedRight (NovDecs) (Radix r)
                     DecDecs (Group r DecDecs) (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)

-- Grouped - less than one

-- | Less than one, first group is zeroes only.  Optional leading
-- zero.

data FracunoFirstGroupZ r =
  FracunoFirstGroupZ (Maybe ZeroDigit) (Radix r)
                     Zeroes (Seq (Group r Zeroes))
                     (Group r ZeroesNovDecs) (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)

-- | Less than one, first group has non-zero digit.  Optional leading
-- zero.
data FracunoFirstGroupNZ r =
  FracunoFirstGroupNZ (Maybe ZeroDigit) (Radix r)
                      ZeroesNovDecs (Group r DecDecs)
                      (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)


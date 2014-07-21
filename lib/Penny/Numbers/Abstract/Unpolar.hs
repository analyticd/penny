{-# LANGUAGE BangPatterns #-}
-- | Unpolar abstract numbers.
module Penny.Numbers.Abstract.Unpolar where

import Deka.Native
import Penny.Numbers.Abstract.RadGroup

data NonNegative = Zero | Plus NonNegative
  deriving (Eq, Ord, Show)

intToNonNegative :: Integral a => a -> Maybe NonNegative
intToNonNegative a
  | a < 0 = Nothing
  | otherwise = Just $ go Zero a
  where
    go soFar i
      | i == 0 = soFar
      | otherwise = go (Plus soFar) (pred i)

data Positive = One | Succ Positive
  deriving (Eq, Ord, Show)

intToPositive :: Integral a => a -> Maybe Positive
intToPositive a
  | a < 1 = Nothing
  | otherwise = Just $ go One a
  where
    go soFar i
      | i == 1 = soFar
      | otherwise = go (Succ soFar) (pred i)

data NovDecs = NovDecs Novem [Decem]
  deriving (Eq, Ord, Show)

data ZeroesNovDecs = ZeroesNovDecs
  { zndZeroes :: NonNegative
  , zndMSD :: Novem
  , zndTrailing :: [Decem]
  } deriving (Eq, Ord, Show)

data DecDecs = DecDecs Decem [Decem]
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

data GZ r g = GZ (Maybe ZeroDigit) (Radix r) Zeroes (Group g Zeroes)
                 [Group g Zeroes]
  deriving (Eq, Ord, Show)

-- Grouped - non-zero

-- Grouped - greater than or equal to one

-- | Greater than or equal to one, grouped on left side.  No radix.
data MasunoGroupedLeft g =
  MasunoGroupedLeft NovDecs (Group g DecDecs) [Group g DecDecs]
  deriving (Eq, Ord, Show)

-- | Greater than or equal to one, grouped on left side, with radix.
-- Optional grouping on right side.
data MasunoGroupedLeftRad r g =
  MasunoGroupedLeftRad (MasunoGroupedLeft g)
                       (Radix r)
                       (Maybe (DecDecs, [Group g DecDecs]))
  deriving (Eq, Ord, Show)

-- | Greater than or equal to one, grouped on right side only.

data MasunoGroupedRight r g =
  MasunoGroupedRight (NovDecs) (Radix r)
                     DecDecs (Group g DecDecs) [Group g DecDecs]
  deriving (Eq, Ord, Show)

-- Grouped - less than one

-- | Less than one, first group is zeroes only.  Optional leading
-- zero.

data FracunoFirstGroupZ r g =
  FracunoFirstGroupZ (Maybe ZeroDigit) (Radix r)
                     Zeroes [Group g Zeroes]
                     (Group g ZeroesNovDecs) [Group g DecDecs]
  deriving (Eq, Ord, Show)

-- | Less than one, first group has non-zero digit.  Optional leading
-- zero.
data FracunoFirstGroupNZ r g =
  FracunoFirstGroupNZ (Maybe ZeroDigit) (Radix r)
                      ZeroesNovDecs (Group g DecDecs)
                      [Group g DecDecs]
  deriving (Eq, Ord, Show)


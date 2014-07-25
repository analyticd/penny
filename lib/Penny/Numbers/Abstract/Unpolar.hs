{-# LANGUAGE BangPatterns #-}
-- | Unpolar abstract numbers.
module Penny.Numbers.Abstract.Unpolar where

import Data.Sequence (Seq)
import Penny.Numbers.Abstract.RadGroup
import Deka.Native (Novem(..), Decem(..))

data NonNegative = Zero | Plus NonNegative
  deriving (Eq, Ord, Show)

{-
-- | Splits a Decuple according to the contents of an exponent.
splitDecuple
  :: Decuple
  -- ^ The exponent
  -> Decuple
  -- ^ Decuple to split
  -> Either (Decuple, Maybe DecDecs) (ZeroesNovDecs)
  -- ^ If the exponent is greater than or equal to the number of
  -- digits in the Decuple to split, returns a Right; otherwise,
  -- returns a Left

splitDecuple expt (Decuple nv dcs)
  = finish $ go exptInt [] (reverse dcs)
  where
    exptInt = decupleToInt expt
    _types = exptInt :: Integer

    go e dsSoFar ds
      | e < 0 = error "splitDecuple: negative"
      | e == 0 = (0, dsSoFar, ds)
      | otherwise = case ds of
          [] -> (e, dsSoFar, [])
          x:xs -> go (pred e) (x : dsSoFar) xs

    finish (e, dsSoFar, dsRem) = case reverse dsRem of
      [] -> case intToNonNegative (pred e) of
        Nothing -> Left (Decuple nv dsSoFar, Nothing)
        Just nn -> Right (ZeroesNovDecs nn nv dsSoFar)
      x:xs -> Left (Decuple nv dsSoFar, Just (DecDecs x xs))
-}

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

data NovDecs = NovDecs
  { ndNovem :: Novem
  , ndDecems :: Seq Decem
  } deriving (Eq, Ord, Show)

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


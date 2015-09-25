{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.NonZero.Internal where

import Penny.Natural.Internal
import Penny.Polar

newtype NonZero = NonZero { nonZeroToInteger :: Integer }
  deriving (Eq, Ord, Show)

instance Polar NonZero where
  polar (NonZero i)
    | i < 0 = negative
    | otherwise = positive
  align pole (NonZero i) = NonZero (changeSign i)
    where
      changeSign
        | pole == negative && i > 0 = negate
        | pole == positive && i < 0 = negate
        | otherwise = id

nonZeroSign :: NonZero -> Pole
nonZeroSign (NonZero i)
  | i < 0 = negative
  | otherwise = positive

integerToNonZero :: Integer -> Maybe NonZero
integerToNonZero i
  | i == 0 = Nothing
  | otherwise = Just . NonZero $ i

addNonZero :: NonZero -> NonZero -> Maybe NonZero
addNonZero (NonZero x) (NonZero y)
  | r == 0 = Nothing
  | otherwise = Just . NonZero $ r
  where
    r = x + y

-- | Strips the sign from the 'NonZero'.
nonZeroToPositive :: NonZero -> Positive
nonZeroToPositive (NonZero i) = Positive (abs i)

-- | Changes a 'Positive' to a positive 'NonZero'.
c'NonZero'Positive :: Positive -> NonZero
c'NonZero'Positive (Positive i) = NonZero i

module Penny.Lincoln.NonZero.Internal where

newtype NonZero = NonZero { nonZeroToInteger :: Integer }
  deriving (Eq, Ord, Show)

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

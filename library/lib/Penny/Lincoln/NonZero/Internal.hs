module Penny.Lincoln.NonZero.Internal where

newtype NonZero = NonZero { nonZeroToInteger :: Integer }
  deriving (Eq, Ord, Show)

integerToNonZero :: Integer -> Maybe NonZero
integerToNonZero i
  | i == 0 = Nothing
  | otherwise = Just . NonZero $ i

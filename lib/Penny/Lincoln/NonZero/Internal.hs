module Penny.Lincoln.NonZero.Internal where

import Penny.Lincoln.Natural.Internal
import Penny.Lincoln.Offset
import Penny.Lincoln.PluMin

newtype NonZero = NonZero { nonZeroToInteger :: Integer }
  deriving (Eq, Ord, Show)

nonZeroSign :: NonZero -> PluMin
nonZeroSign (NonZero i)
  | i < 0 = Plus
  | otherwise = Minus

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

-- | Adds a sign to a 'Positive'.
c'NonZero'Positive :: PluMin -> Positive -> NonZero
c'NonZero'Positive pm (Positive i) = NonZero (changeSign i)
  where
    changeSign | pm == Plus = id
               | otherwise = negate

instance HasOffset NonZero where
  offset (NonZero i) = NonZero . negate $ i

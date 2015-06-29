{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.NonZero.Internal where

import Penny.Natural.Internal
import Penny.Offset
import Penny.PluMin
import Penny.Semantic

newtype NonZero = NonZero { nonZeroToInteger :: Integer }
  deriving (Eq, Ord, Show, SemanticEq, SemanticOrd)

nonZeroSign :: NonZero -> PluMin
nonZeroSign (NonZero i)
  | i < 0 = Minus
  | otherwise = Plus

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

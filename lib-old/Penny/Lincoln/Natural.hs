-- | Natural numbers, either positive or non-zero.  These wrap Int
-- rather than Integers so do not use them were unlimited precision is
-- needed.

module Penny.Lincoln.Natural
  ( NonNegative (unNonNegative)
  , nonNegative
  , Positive (unPositive)
  , positive
  ) where

newtype NonNegative = NonNegative { unNonNegative :: Int }
  deriving (Eq, Show, Ord)

nonNegative :: Int -> Maybe NonNegative
nonNegative i = if i >= 0 then Just (NonNegative i) else Nothing

newtype Positive = Positive { unPositive :: Int }
  deriving (Eq, Show, Ord)

positive :: Int -> Maybe Positive
positive i = if i > 0 then Just (Positive i) else Nothing

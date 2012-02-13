-- | Number-like classes.
module Penny.Lincoln.Classes where

class (Eq a, Ord a) => NonNegative a where
  add :: a -> a -> a
  subt :: a -> a -> Maybe a
  mult :: a -> a -> a
  zero :: a
  fromInt :: Integral f => f -> Maybe a
  unsafeFromInt :: Integral f => f -> a

class NonNegative a => NonNegInt a where
  toInt :: (Integral t) => a -> t

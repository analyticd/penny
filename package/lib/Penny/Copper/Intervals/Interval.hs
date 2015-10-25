-- | Closed-ended intervals.
module Penny.Copper.Intervals.Interval
  ( Interval
  , singleton
  , range
  , endLeft
  , endRight
  ) where

-- | A closed-ended interval.
data Interval a = Interval a a
  deriving (Eq, Ord, Show)

-- | Creates an 'Interval' with identical endpoints.
singleton :: a -> Interval a
singleton a = Interval a a

-- | Creates an 'Interval' with two different endpoints.  The interval
-- is closed ended.  You can specify the endpoints in any order; that
-- is:
--
-- @
-- range a b == range b a
-- @
range :: Ord a => a -> a -> Interval a
range a b = Interval (min a b) (max a b)

instance Functor Interval where
  fmap f (Interval l r) = Interval (f l) (f r)

-- | The left endpoint of an 'Interval'.
endLeft :: Interval a -> a
endLeft (Interval a _) = a

-- | The right endpoint of an 'Interval'.
endRight :: Interval a -> a
endRight (Interval _ b) = b


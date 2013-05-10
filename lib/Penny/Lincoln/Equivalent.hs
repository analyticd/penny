module Penny.Lincoln.Equivalent where

-- | Comparisons for equivalency. Two items are equivalent if they
-- have the same semantic meaning, even if the data in the two items
-- is different.
class Equivalent a where
  equivalent :: a -> a -> Bool

(==~) :: Equivalent a => a -> a -> Bool
(==~) = equivalent
infix 4 ==~

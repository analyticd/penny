module Penny.Lincoln.Equivalent where

import Data.Monoid ((<>))

-- | Comparisons for equivalency. Two items are equivalent if they
-- have the same semantic meaning, even if the data in the two items
-- is different.
class Equivalent a where
  equivalent :: a -> a -> Bool

  -- | Compares based on equivalency.
  compareEv :: a -> a -> Ordering

(==~) :: Equivalent a => a -> a -> Bool
(==~) = equivalent
infix 4 ==~

instance (Equivalent a, Equivalent b) => Equivalent (a, b) where
  equivalent (a1, b1) (a2, b2) = a1 ==~ a2 && b1 ==~ b2
  compareEv (a1, b1) (a2, b2) =
    compareEv a1 a2 <> compareEv b1 b2

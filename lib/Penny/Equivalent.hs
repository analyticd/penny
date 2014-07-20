module Penny.Equivalent where

import Data.Monoid ((<>))

-- | Comparisons for equivalency. Two items are equivalent if they
-- have the same semantic meaning, even if the data in the two items
-- is different.
class Equivalent a where
  -- | Compares based on equivalency.
  compareEv :: a -> a -> Ordering

  equivalent :: a -> a -> Bool
  equivalent x y = compareEv x y == EQ


(==~) :: Equivalent a => a -> a -> Bool
(==~) = equivalent
infix 4 ==~

instance Equivalent a => Equivalent (Maybe a) where
  equivalent a b = case (a, b) of
    (Just x, Just y) -> x ==~ y
    (Nothing, Nothing) -> True
    _ -> False
  compareEv a b = case (a, b) of
    (Just x, Just y) -> compareEv x y
    (Nothing, Nothing) -> EQ
    (Just _, Nothing) -> GT
    (Nothing, Just _) -> LT

instance (Equivalent a, Equivalent b) => Equivalent (a, b) where
  equivalent (a1, b1) (a2, b2) = a1 ==~ a2 && b1 ==~ b2
  compareEv (a1, b1) (a2, b2) =
    compareEv a1 a2 <> compareEv b1 b2

instance (Equivalent a, Equivalent b, Equivalent c) =>
  Equivalent (a, b, c) where
    equivalent (a1, b1, c1) (a2, b2, c2) = a1 ==~ a2
      && b1 ==~ b2 && c1 ==~ c2
    compareEv (a1, b1, c1) (a2, b2, c2) =
      compareEv a1 a2 <> compareEv b1 b2 <> compareEv c1 c2

instance (Equivalent a, Equivalent b) => Equivalent (Either a b) where
  equivalent e1 e2 = case (e1, e2) of
    (Left l1, Left l2) -> l1 ==~ l2
    (Right r1, Right r2) -> r1 ==~ r2
    _ -> False
  compareEv e1 e2 = case (e1, e2) of
    (Left l1, Left l2) -> compareEv l1 l2
    (Right l1, Right l2) -> compareEv l1 l2
    (Left _, Right _) -> LT
    (Right _, Left _) -> GT

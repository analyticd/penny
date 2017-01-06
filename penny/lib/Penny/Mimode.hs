-- | The \"mimode\" is a modified statistical mode.
module Penny.Mimode where

import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Foldable as F

-- | Returns a list of modes, sorted with the biggest modes first.
modes
  :: (a -> a -> Ordering)
  -> [a]
  -> [(Int, a)]
modes cmp
  = sortBy (flip (comparing fst))
  . map (\ls -> (length ls, head ls))
  . groupBy getEq
  . sortBy cmp
  where
    getEq x y
      | cmp x y == EQ = True
      | otherwise = False

-- | Non-overloaded version of 'mimode'.
mimodeBy
  :: F.Foldable f
  => (a -> a -> Ordering)
  -> f a
  -> Maybe a
mimodeBy cmp = fmap snd . safeHead . modes cmp . F.toList
  where
    safeHead (x:_) = Just x
    safeHead [] = Nothing

-- | Computes the \"mimode\" of a foldable structure.
--
-- The so-called \"mimode\" of a list @ls@ is the value appearing most
-- frequently in @ls@, if there is one.  If there is more than one
-- value that appears most frequently, one is chosen arbitrarily.  A
-- list has no mimode only if the list is empty.  For instance, the
-- mimode of @[1,2,3]@ may be any of 1, 2, or 3; the mimode of
-- @[1,1,2,3]@ is @1@; and @[]@ has no mimode.
mimode
  :: (Ord a, F.Foldable f)
  => f a
  -> Maybe a
mimode = mimodeBy compare

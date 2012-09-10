-- | The Convert report.

module Penny.Cabin.Balance.Convert where

import qualified Data.List as L
import Data.Ord (comparing)

-- | Like lastModeBy but using Ord.
lastMode :: Ord a => [a] -> Maybe a
lastMode = lastModeBy compare

-- | Finds the mode of a list. Takes the mode that is located last in
-- the list. Returns Nothing if there is no mode (that is, if the list
-- is empty).
lastModeBy ::
  (a -> a -> Ordering)
  -> [a]
  -> Maybe a
lastModeBy o ls =
  case modesBy o' ls' of
    [] -> Nothing
    ms -> Just . fst . L.maximumBy fx $ ms
    where
      fx = comparing snd
      ls' = zip ls ([0..] :: [Int])
      o' x y = o (fst x) (fst y)

-- | Like modesBy but using Ord.
modes :: Ord a => [a] -> [a]
modes = modesBy compare

-- | Finds the modes of a list.
modesBy :: (a -> a -> Ordering) -> [a] -> [a]
modesBy o =
  concat
  . longestLists
  . L.groupBy (\x y -> o x y == EQ)
  . L.sortBy o
  

-- | Returns the longest lists.
longestLists :: [[a]] -> [[a]]
longestLists as =
  let lengths = map (\ls -> (ls, length ls)) as
      maxLen = maximum . map snd $ lengths
  in map fst . filter (\(_, len) -> len == maxLen) $ lengths

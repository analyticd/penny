-- | The \"mimode\" is a modified statistical mode.
module Penny.Mimode where

import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import Data.Maybe

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
mimode = finish . M.toList . F.foldl' fldr M.empty
  where
    fldr mp val = case M.lookup val mp of
      Nothing -> M.insert val 1 mp
      Just old -> M.insert val (succ old) mp
    finish pairs = listToMaybe . map fst . filter isMaxPair $ pairs
      where
        isMaxPair (_, v) = v == maxVal
          where
            maxVal = maximum . ((0 :: Integer) :) . map snd $ pairs



module Penny.Cabin.Posts.Meta (
  M.VisibleNum(M.unVisibleNum)
  , PostMeta(filteredNum, sortedNum, visibleNum)
  , addPostMeta
  ) where

import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Cabin.Meta as M

data PostMeta =
  PostMeta { filteredNum :: Ly.FilteredNum
            , sortedNum :: Ly.SortedNum
            , visibleNum :: M.VisibleNum }
  deriving Show

-- | Applied to a list of Box that have already been filtered, returns
-- a list of Box with posting metadata.
addPostMeta :: [L.Box Ly.LibertyMeta] -> [L.Box PostMeta]
addPostMeta = M.visibleNums f where
  f vn lm = PostMeta (Ly.filteredNum lm) (Ly.sortedNum lm) vn

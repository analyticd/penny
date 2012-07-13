module Penny.Cabin.Posts.Meta where

import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly

-- | A serial assigned to each posting after invisible posts are
-- thrown out.
newtype VisibleNum = VisibleNum { unVisible :: L.Serial }
                   deriving Show

data PostMeta =
  PostMeta { filteredNum :: Ly.FilteredNum
            , sortedNum :: Ly.SortedNum
            , visibleNum :: VisibleNum }
  deriving Show

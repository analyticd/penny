module Penny.Harvest.Collect.AfterPosting where

import qualified Penny.Harvest.Collect.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collect.PostingBox as PostingBox
import Data.Sequence (Seq)

-- | At least one posting has already been seen; currently
-- processing additional postings or memo lines.

data T = T
  { topLine :: AfterTopLine.T
  , postings :: (Seq PostingBox.T)
  , last :: PostingBox.T
  } deriving (Eq, Ord, Show)

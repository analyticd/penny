module Penny.Harvest.Collected.AfterPosting where

import qualified Penny.Harvest.Collected.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collected.PostingBox as PostingBox
import Data.Sequence (Seq)

-- | At least one posting has already been seen; currently
-- processing additional postings or memo lines.

data T = T
  { topLine :: AfterTopLine.T
  , postings :: (Seq PostingBox.T)
  , last :: PostingBox.T
  } deriving (Eq, Ord, Show)

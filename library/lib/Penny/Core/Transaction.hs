module Penny.Core.Transaction where

import qualified Penny.Core.TopLine as TopLine
import qualified Penny.Core.Balanced as Balanced
import qualified Penny.Core.Posting as Posting

data T = T
  { topLine :: TopLine.T
  , postings :: Balanced.T Posting.T
  } deriving (Eq, Ord, Show)

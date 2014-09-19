module Penny.Lincoln.Bundle where

import qualified Penny.Lincoln.TopLine as TopLine
import qualified Penny.Lincoln.View as View
import qualified Penny.Lincoln.Posting as Posting

data T = T
  { topLine :: TopLine.T
  , postings :: View.T Posting.T
  } deriving (Eq, Ord, Show)

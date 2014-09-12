module Penny.Bundle where

import qualified Penny.TopLine as TopLine
import qualified Penny.View as View
import qualified Penny.Posting as Posting

data T = T
  { topLine :: TopLine.T
  , postings :: View.T Posting.T
  } deriving (Eq, Ord, Show)

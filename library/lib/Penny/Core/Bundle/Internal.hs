module Penny.Core.Bundle.Internal where

import qualified Penny.Core.TopLine as TopLine
import qualified Penny.Core.View as View
import qualified Penny.Core.Posting as Posting

-- | A a single balanced posting, with its associated
-- 'Penny.Core.TopLine.T' and sibling postings.
data T = T
  { topLine :: TopLine.T
  , postings :: View.T Posting.T
  } deriving (Eq, Ord, Show)


module Penny.Core.Bundle where

import qualified Penny.Core.TopLine as TopLine
import qualified Penny.Core.View as View
import qualified Penny.Core.Posting as Posting
import qualified Penny.Core.Transaction as Transaction
import Data.Sequence (Seq)

data T = T
  { topLine :: TopLine.T
  , postings :: View.T Posting.T
  } deriving (Eq, Ord, Show)

fromTransaction :: Transaction.T -> Seq T
fromTransaction

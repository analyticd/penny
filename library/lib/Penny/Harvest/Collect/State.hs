module Penny.Harvest.Collect.State where

import qualified Penny.Harvest.Collect.Memo.Transaction as Memos
import qualified Penny.Harvest.Collect.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collect.AfterPosting as AfterPosting

data T
  = Empty
  -- ^ Not currently in posting or transaction; awaiting next input
  | TxnMemos Memos.T
  | AfterTopLine AfterTopLine.T
  | AfterPosting AfterPosting.T
  deriving (Eq, Ord, Show)

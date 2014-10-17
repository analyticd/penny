module Penny.Harvest.Collected.State where

import qualified Penny.Harvest.Collected.Memo.Transaction as Memos
import qualified Penny.Harvest.Collected.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collected.AfterPosting as AfterPosting

data T
  = Empty
  -- ^ Not currently in posting or transaction; awaiting next input
  | TxnMemos Memos.T
  | AfterTopLine AfterTopLine.T
  | AfterPosting AfterPosting.T
  deriving (Eq, Ord, Show)

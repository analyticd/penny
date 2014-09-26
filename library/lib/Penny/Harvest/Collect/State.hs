module Penny.Harvest.Collect.State where

import qualified Penny.Harvest.Collect.TransactionMemos as TransactionMemos
import qualified Penny.Harvest.Collect.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collect.AfterPosting as AfterPosting

data T
  = Empty
  -- ^ Not currently in posting or transaction; awaiting next input
  | TxnMemos TransactionMemos.T
  | AfterTopLine AfterTopLine.T
  | AfterPosting AfterPosting.T
  deriving (Eq, Ord, Show)

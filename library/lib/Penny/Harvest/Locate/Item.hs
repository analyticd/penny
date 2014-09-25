module Penny.Harvest.Locate.Item where

import qualified Penny.Tree.Memo.Transaction as MemoT
import qualified Penny.Tree.Memo.Posting as MemoP
import qualified Penny.Tree.TopLine as TopLine
import qualified Penny.Tree.Posting as Posting

data T
  = T0 MemoT.T
  | T1 TopLine.T
  | T2 Posting.T
  | T3 MemoP.T
  deriving (Eq, Ord, Show)

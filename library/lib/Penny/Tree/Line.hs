module Penny.Tree.Line where

import qualified Penny.Tree.Comment as Comment
import qualified Penny.Tree.BlankLine as BlankLine
import qualified Penny.Tree.Price as Price
import qualified Penny.Tree.Memo.Transaction as Memo.Transaction
import qualified Penny.Tree.Memo.Posting as Memo.Posting
import qualified Penny.Tree.TopLine as TopLine
import qualified Penny.Tree.Posting as Posting

data T
  = T0 Comment.T
  | T1 BlankLine.T
  | T2 Price.T
  | T3 Memo.Transaction.T
  | T4 TopLine.T
  | T5 Posting.T
  | T6 Memo.Posting.T
  deriving (Eq, Ord, Show)

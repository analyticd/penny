module Penny.Copper.Tree.Line where

import Penny.Copper.Tree.Posting
import Penny.Copper.Tree.TopLine
import Penny.Copper.Tree.Price
import Penny.Copper.Tree.BlankLine
import qualified Penny.Copper.Tree.Memo.Posting as MP
import qualified Penny.Copper.Tree.Memo.Transaction as MT
import Penny.Copper.Tree.Comment

data Line
  = L0 Comment
  | L1 BlankLine
  | L2 Price
  | L3 MT.Memo
  | L4 TopLine
  | L5 Posting
  | L6 MP.Memo
  deriving (Eq, Ord, Show)

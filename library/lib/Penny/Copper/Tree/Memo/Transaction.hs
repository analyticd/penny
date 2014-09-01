module Penny.Copper.Tree.Memo.Transaction where

import Penny.Copper.Tree.Tokens
import Data.Sequence (Seq)
import Penny.Copper.Tree.NonNewline

data Memo = Memo Semicolon (Seq NonNewline) Newline
  deriving (Eq, Ord, Show)

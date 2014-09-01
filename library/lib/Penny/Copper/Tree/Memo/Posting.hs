module Penny.Copper.Tree.Memo.Posting where

import Penny.Copper.Tree.Tokens
import Penny.Copper.Tree.PreSpace
import Penny.Copper.Tree.NonNewline
import Data.Sequence (Seq)

data Memo = Memo (PreSpace (Semicolon, (Seq NonNewline), Newline))
  deriving (Eq, Ord, Show)


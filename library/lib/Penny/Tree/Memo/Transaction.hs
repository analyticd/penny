module Penny.Tree.Memo.Transaction where

import Data.Sequence (Seq)
import qualified Penny.Tree.NonNewline as NonNewline
import qualified Penny.Tree.Newline as Newline
import qualified Penny.Tree.Semicolon as Semicolon

data T = T Semicolon.T (Seq NonNewline.T) Newline.T
  deriving (Eq, Ord, Show)

module Penny.Tree.Memo.Posting where

import Data.Sequence (Seq)
import qualified Penny.Tree.NonNewline as NonNewline
import qualified Penny.Tree.Newline as Newline
import qualified Penny.Tree.Semicolon as Semicolon
import qualified Penny.Tree.Spaces as Spaces

data T = T Spaces.T Semicolon.T (Seq NonNewline.T) Newline.T
  deriving (Eq, Ord, Show)

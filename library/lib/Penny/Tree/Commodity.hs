module Penny.Tree.Commodity where

import qualified Penny.Tree.Caret as Caret
import qualified Penny.Tree.Commodity.Char as CharC
import Data.Sequence (Seq)

data T = T Caret.T (Seq CharC.T) Caret.T
  deriving (Eq, Ord, Show)

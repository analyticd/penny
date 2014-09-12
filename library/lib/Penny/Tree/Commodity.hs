module Penny.Tree.Commodity where

import qualified Penny.Caret as Caret
import qualified Penny.Char.Commodity as CharC
import Data.Sequence (Seq)

data T = T Caret.T (Seq CharC.T) Caret.T
  deriving (Eq, Ord, Show)

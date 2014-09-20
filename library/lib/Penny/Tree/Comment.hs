module Penny.Tree.Comment where

import qualified Penny.Tree.Comment.Char as Char
import qualified Penny.Tree.Octothorpe as Octothorpe
import Data.Sequence (Seq)
import qualified Penny.Tree.Newline as Newline

data T = T Octothorpe.T (Seq Char.T) Newline.T
  deriving (Eq, Ord, Show)

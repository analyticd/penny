module Penny.Tree.BlankLine where

import qualified Penny.Tree.Newline as Newline

newtype T = T Newline.T
  deriving (Eq, Ord, Show)

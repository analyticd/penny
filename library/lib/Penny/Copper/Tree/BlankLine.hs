module Penny.Copper.Tree.BlankLine where

import Penny.Copper.Tree.Tokens

data BlankLine = BlankLine Newline
  deriving (Eq, Ord, Show)

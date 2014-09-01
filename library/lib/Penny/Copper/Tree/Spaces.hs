module Penny.Copper.Tree.Spaces where

import Penny.Numbers.Natural

newtype Spaces = Spaces { unSpaces :: Pos }
  deriving (Eq, Ord, Show)

module Penny.Copper.Tree.File where

import Penny.Copper.Tree.Line
import Data.Sequence (Seq)
import Penny.Copper.Tree.Tokens

data File = File (Seq Line) EOF
  deriving (Eq, Ord, Show)

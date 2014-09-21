module Penny.Tree.Colon where

import Penny.Tree.Parsec

data T = T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$ char ':'

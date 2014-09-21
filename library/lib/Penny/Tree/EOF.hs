module Penny.Tree.EOF where

import Penny.Tree.Parsec

data T = T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$ eof <?> "end of file"

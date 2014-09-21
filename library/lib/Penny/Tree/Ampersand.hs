module Penny.Tree.Ampersand where

import Penny.Tree.Parsec

data T = T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$ char '&'

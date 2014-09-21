module Penny.Tree.Square.Close where

import Penny.Tree.Parsec

data T = T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = fmap (const T) $ char ']'

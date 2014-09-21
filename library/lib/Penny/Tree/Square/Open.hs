module Penny.Tree.Square.Open where

import Penny.Tree.Parsec

data T = T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = fmap (const T) $ char '['

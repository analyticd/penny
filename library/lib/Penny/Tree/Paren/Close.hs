module Penny.Tree.Paren.Close where

import Text.Parsec.Text
import Text.Parsec (char)

data T = T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = fmap (const T) $ char ')'

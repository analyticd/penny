module Penny.Core.Anna.Zero where

import Text.Parsec.Text
import Text.Parsec.Char

data T = T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = fmap (const T) $ char '0'

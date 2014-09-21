module Penny.Core.Anna.RadPer where

import Text.Parsec.Char
import Text.Parsec.Text

data T = T
  deriving (Eq, Ord, Show)

comma :: T
comma = T

parser :: Parser T
parser = fmap (const T) $ char ','

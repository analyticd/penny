module Penny.Core.Anna.RadCom where

import Text.Parsec.Text
import Text.Parsec.Char

data T = T
  deriving (Eq, Ord, Show)

period :: T
period = T

parser :: Parser T
parser = fmap (const T) $ char '.'

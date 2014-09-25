module Penny.Core.Anna.RadPer where

import Text.Parsec.Char
import Text.Parsec.Text
import qualified Penny.Core.Anna.Radix as Radix

data T = T
  deriving (Eq, Ord, Show)

comma :: T
comma = T

parser :: Parser T
parser = fmap (const T) $ char ','

parseRadix :: Parser (Radix.T T)
parseRadix = fmap (const Radix.T) $ char '.'

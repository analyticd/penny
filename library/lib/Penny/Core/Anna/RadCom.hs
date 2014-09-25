module Penny.Core.Anna.RadCom where

import Text.Parsec.Text
import Text.Parsec.Char
import qualified Penny.Core.Anna.Radix as Radix

data T = T
  deriving (Eq, Ord, Show)

period :: T
period = T

parser :: Parser T
parser = fmap (const T) $ char '.'

parseRadix :: Parser (Radix.T T)
parseRadix = fmap (const Radix.T) $ char ','

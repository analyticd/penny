module Penny.Tree.SubAccount.Unquoted.Char.Next
  ( T
  , toChar
  , fromChar
  , parser
  ) where

import Text.Parsec.Text
import Penny.Tree.Parsec (accept)

data T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= ':' && c /= '\n' && c /= ' ' = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "unquoted sub-account letter" fromChar

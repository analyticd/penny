module Penny.Tree.Payee.TopLine.Char.First
  ( T
  , toChar
  , fromChar
  , parser
  ) where

import Penny.Tree.Parsec (accept)
import Text.Parsec.Text

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= '{' && c /= '[' && c /= '(' && c /= '\n' && c /= ' '
      = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "top-line posting first character" fromChar

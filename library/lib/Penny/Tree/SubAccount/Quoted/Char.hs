module Penny.Tree.SubAccount.Quoted.Char
  ( T
  , toChar
  , fromChar
  , parser
  ) where

import Penny.Tree.Parsec (accept)
import Text.Parsec.Text

data T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= ':' && c /= '\n' && c /= '}' = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "sub-account character" fromChar

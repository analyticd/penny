module Penny.Tree.Payee.Posting.Char
  ( T
  , toChar
  , fromChar
  , parser
  ) where

import Text.Parsec.Text
import Penny.Tree.Parsec (accept)

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= '\n' && c /= '~' = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "posting character" fromChar

module Penny.Tree.Flag.Char
  ( T
  , toChar
  , fromChar
  , parser
  ) where

import Penny.Tree.Parsec

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= ']' && c /= '\n' = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "flag character" fromChar

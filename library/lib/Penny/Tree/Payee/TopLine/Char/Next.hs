module Penny.Tree.Payee.TopLine.Char.Next
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
  | c /= '\n' = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "top-line character - non-newline" fromChar

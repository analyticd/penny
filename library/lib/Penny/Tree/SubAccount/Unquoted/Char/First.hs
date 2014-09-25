module Penny.Tree.SubAccount.Unquoted.Char.First
  ( T
  , toChar
  , fromChar
  , parser
  ) where

import Data.Char
import Penny.Tree.Parsec (accept)
import Text.Parsec.Text

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | isLetter c = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "unquoted sub-account letter" fromChar

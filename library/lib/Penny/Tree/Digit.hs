module Penny.Tree.Digit
  ( T
  , toChar
  , fromChar
  , toInt
  ) where

import Data.Char

data T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | isDigit c = Just $ T c
  | otherwise = Nothing

toInt :: T -> Int
toInt (T c) = digitToInt c

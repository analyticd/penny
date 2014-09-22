module Penny.Tree.SubAccount.Unquoted.Char.Next
  ( T
  , toChar
  , fromChar
  ) where

data T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= ':' && c /= '\n' && c /= ' ' = Just $ T c
  | otherwise = Nothing
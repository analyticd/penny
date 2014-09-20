module Penny.Tree.Number.Char
  ( T
  , toChar
  , fromChar
  ) where

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= ')' && c /= '\n' = Just $ T c
  | otherwise = Nothing

module Penny.Tree.NonNewline
  ( T
  , toChar
  , fromChar
  ) where

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= '\n' = Just $ T c
  | otherwise = Nothing

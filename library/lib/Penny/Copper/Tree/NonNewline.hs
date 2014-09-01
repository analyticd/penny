module Penny.Copper.Tree.NonNewline
  ( NonNewline
  , unNonNewline
  , nonNewline
  ) where

newtype NonNewline = NonNewline { unNonNewline :: Char }
  deriving (Eq, Ord, Show)

nonNewline :: Char -> Maybe NonNewline
nonNewline c
  | c /= '\n' = Just $ NonNewline c
  | otherwise = Nothing

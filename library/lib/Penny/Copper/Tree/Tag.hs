module Penny.Copper.Tree.Tag
  ( TagChar
  , unTagChar
  , tagChar
  , Tag(..)
  ) where

import Penny.Copper.Tree.Tokens
import Data.Sequence (Seq)

newtype TagChar = TagChar { unTagChar :: Char }
  deriving (Eq, Ord, Show)

tagChar :: Char -> Maybe TagChar
tagChar c
  | c /= '\n' && c /= ' ' = Just $ TagChar c
  | otherwise = Nothing

data Tag = Tag Asterisk (Seq TagChar)
  deriving (Eq, Ord, Show)

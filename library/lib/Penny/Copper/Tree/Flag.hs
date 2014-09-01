module Penny.Copper.Tree.Flag
  ( Flag(..)
  , FlagChar
  , unFlagChar
  , flagChar
  ) where

import Penny.Copper.Tree.Tokens
import Data.Sequence (Seq)

data Flag = Flag OpenSquare (Seq FlagChar) CloseSquare
  deriving (Eq, Ord, Show)

data FlagChar = FlagChar { unFlagChar :: Char }
  deriving (Eq, Ord, Show)

flagChar :: Char -> Maybe FlagChar
flagChar c
  | c /= ']' && c /= '\n' = Just $ FlagChar c
  | otherwise = Nothing

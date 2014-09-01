module Penny.Copper.Tree.Number
  ( Number(..)
  , NumberChar
  , unNumberChar
  , numberChar
  ) where

import Penny.Copper.Tree.Tokens
import Data.Sequence (Seq)

data Number = Number OpenParen (Seq NumberChar) CloseParen
  deriving (Eq, Ord, Show)

newtype NumberChar = NumberChar { unNumberChar :: Char }
  deriving (Eq, Ord, Show)

numberChar :: Char -> Maybe NumberChar
numberChar c
  | c /= ')' && c /= '\n' = Just $ NumberChar c
  | otherwise = Nothing

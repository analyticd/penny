module Penny.Copper.Tree.Account.Unquoted
  ( FirstChar
  , unFirstChar
  , firstChar
  , NextChar
  , unNextChar
  , nextChar
  , FirstSubAccount(..)
  , NextSubAccount(..)
  , Account(..)
  ) where

import Penny.Copper.Tree.Tokens
import Penny.Numbers.Natural
import Data.Char

newtype NextChar = NextChar { unNextChar :: Char }
  deriving (Eq, Ord, Show)

nextChar :: Char -> Maybe NextChar
nextChar c
  | c /= ':' && c /= '\n' && c /= ' ' = Just $ NextChar c
  | otherwise = Nothing

newtype FirstChar = FirstChar { unFirstChar :: Char }
  deriving (Eq, Ord, Show)

firstChar :: Char -> Maybe FirstChar
firstChar c
  | isLetter c = Just $ FirstChar c
  | otherwise = Nothing

newtype FirstSubAccount = FirstSubAccount (NE FirstChar NextChar)
  deriving (Eq, Ord, Show)

newtype NextSubAccount = NextSubAccount (NE NextChar NextChar)
  deriving (Eq, Ord, Show)

newtype Account
  = Account (NE FirstSubAccount (Colon, NextSubAccount))
  deriving (Eq, Ord, Show)


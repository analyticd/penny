module Penny.Tree.SubAccount.Unquoted.Char.First
  ( T
  , toChar
  , fromChar
  ) where

import Data.Char

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | isLetter c = Just $ T c
  | otherwise = Nothing

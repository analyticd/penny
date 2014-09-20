module Penny.Tree.Payee.Posting.Char
  ( T
  , toChar
  , fromChar
  ) where

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= '\n' && c /= '~' = Just $ T c
  | otherwise = Nothing

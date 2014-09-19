module Penny.Tree.Currency
  ( T
  , toChar
  , fromChar
  ) where

import Data.Char

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | generalCategory c == CurrencySymbol = Just $ T c
  | otherwise = Nothing

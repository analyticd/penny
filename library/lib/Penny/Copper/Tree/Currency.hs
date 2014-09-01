module Penny.Copper.Tree.Currency
  (Currency, unCurrency, currency) where

import Data.Char

newtype Currency = Currency { unCurrency :: Char }
  deriving (Eq, Ord, Show)

currency :: Char -> Maybe Currency
currency c
  | generalCategory c == CurrencySymbol = Just $ Currency c
  | otherwise = Nothing

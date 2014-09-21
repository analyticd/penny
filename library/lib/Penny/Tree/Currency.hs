module Penny.Tree.Currency
  ( T
  , toChar
  , fromChar
  , parser
  ) where

import Data.Char
import Penny.Tree.Parsec

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | generalCategory c == CurrencySymbol = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = accept "currency symbol" fromChar

module Penny.Copper.Tree.Digit
  ( Digit
  , unDigit
  , digit
  ) where

import Data.Char

newtype Digit = Digit { unDigit :: Char }
  deriving (Eq, Ord, Show)

digit :: Char -> Maybe Digit
digit c
  | isDigit c = Just $ Digit c
  | otherwise = Nothing

module Penny.Copper.Tree.Digit
  ( Digit
  , unDigit
  , digit
  , digitToInt
  ) where

import Data.Char (isDigit)

newtype Digit = Digit { unDigit :: Char }
  deriving (Eq, Ord, Show)

digit :: Char -> Maybe Digit
digit c
  | isDigit c = Just $ Digit c
  | otherwise = Nothing

digitToInt :: Integral a => Digit -> a
digitToInt (Digit c) = case c of
  { '0' -> 0; '1' -> 1; '2' -> 2; '3' -> 3; '4' -> 4; '5' -> 5;
    '6' -> 6; '7' -> 7; '8' -> 8; '9' -> 9;
    _ -> error "digitToInt: error" }

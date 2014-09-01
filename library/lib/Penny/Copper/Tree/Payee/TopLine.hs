module Penny.Copper.Tree.Payee.TopLine
  ( Payee(..)
  , PayeeFirstChar
  , PayeeNextChar
  , unPayeeFirstChar
  , unPayeeNextChar
  , payeeFirstChar
  , payeeNextChar
  ) where

import Penny.Numbers.Natural

newtype PayeeFirstChar = PayeeFirstChar { unPayeeFirstChar :: Char }
  deriving (Eq, Ord, Show)

payeeFirstChar :: Char -> Maybe PayeeFirstChar
payeeFirstChar c
  | c /= '{' && c /= '[' && c /= '(' && c /= '\n' && c /= ' '
      = Just $ PayeeFirstChar c
  | otherwise = Nothing

newtype PayeeNextChar = PayeeNextChar { unPayeeNextChar :: Char }
  deriving (Eq, Ord, Show)

payeeNextChar :: Char -> Maybe PayeeNextChar
payeeNextChar c
  | c /= '\n' = Just $ PayeeNextChar c
  | otherwise = Nothing

newtype Payee = Payee { unPayee :: NE PayeeFirstChar PayeeNextChar }
  deriving (Eq, Ord, Show)

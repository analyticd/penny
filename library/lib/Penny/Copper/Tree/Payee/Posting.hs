module Penny.Copper.Tree.Payee.Posting
  ( Payee(..)
  , PayeeChar
  , unPayeeChar
  , payeeChar
  ) where

import Penny.Copper.Tree.Tokens
import Data.Sequence (Seq)

data Payee = Payee Tilde (Seq PayeeChar) Tilde
  deriving (Eq, Ord, Show)

newtype PayeeChar = PayeeChar { unPayeeChar :: Char }
  deriving (Eq, Ord, Show)

payeeChar :: Char -> Maybe PayeeChar
payeeChar c
  | c /= '\n' && c /= '~' = Just $ PayeeChar c
  | otherwise = Nothing


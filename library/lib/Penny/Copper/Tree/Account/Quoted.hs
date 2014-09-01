module Penny.Copper.Tree.Account.Quoted
  ( SubAccountChar
  , unSubAccountChar
  , subAccountChar
  , SubAccount(..)
  , Account(..)
  ) where

import Penny.Copper.Tree.Tokens
import Penny.Numbers.Natural
import Data.Sequence (Seq)

newtype SubAccountChar = SubAccountChar { unSubAccountChar :: Char }
  deriving (Eq, Ord, Show)

subAccountChar :: Char -> Maybe SubAccountChar
subAccountChar c
  | c /= ':' && c /= '\n' && c /= '}' = Just $ SubAccountChar c
  | otherwise = Nothing

newtype SubAccount = SubAccount (Seq SubAccountChar)
  deriving (Eq, Ord, Show)

data Account
  = Account OpenCurly (NE SubAccount (Colon, SubAccount)) CloseCurly
  deriving (Eq, Ord, Show)

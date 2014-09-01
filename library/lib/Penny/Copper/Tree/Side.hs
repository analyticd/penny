module Penny.Copper.Tree.Side where

import Penny.Copper.Tree.Tokens

data Debit = Debit LessThan
  deriving (Eq, Ord, Show)

data Credit = Credit GreaterThan
  deriving (Eq, Ord, Show)

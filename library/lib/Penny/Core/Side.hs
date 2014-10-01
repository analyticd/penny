module Penny.Core.Side where

import Deka.Dec

data T
  = Debit
  | Credit
  deriving (Eq, Ord, Show)

opposite :: T -> T
opposite Debit = Credit
opposite Credit = Debit

fromSign :: Sign -> T
fromSign Sign0 = Debit
fromSign Sign1 = Credit

toSign :: T -> Sign
toSign Debit = Sign0
toSign Credit = Sign1

module Penny.Side where

import Deka.Dec

data T
  = Debit
  | Credit
  deriving (Eq, Ord, Show)

flip :: T -> T
flip Debit = Credit
flip Credit = Debit

fromSign :: Sign -> T
fromSign Sign0 = Debit
fromSign Sign1 = Credit

toSign :: T -> Sign
toSign Debit = Sign0
toSign Credit = Sign1

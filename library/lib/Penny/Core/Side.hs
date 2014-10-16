module Penny.Core.Side where

import qualified Penny.Core.Sign as Sign

data T
  = Debit
  | Credit
  deriving (Eq, Ord, Show)

opposite :: T -> T
opposite Debit = Credit
opposite Credit = Debit

fromSign :: Sign.T -> T
fromSign Sign.Pos = Debit
fromSign Sign.Neg = Credit

toSign :: T -> Sign.T
toSign Debit = Sign.Pos
toSign Credit = Sign.Neg

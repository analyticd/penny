module Penny.Lincoln.Decimal.Side where

data Side
  = Debit
  | Credit
  deriving (Eq, Ord, Show)

-- | 'Debit' returns 'Credit'; 'Credit' returns 'Debit'
opposite :: Side -> Side
opposite a = case a of
  Debit -> Credit
  Credit -> Debit

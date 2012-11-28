module Penny.Lincoln.Bits.DrCr where

data DrCr = Debit | Credit deriving (Eq, Show, Ord)

-- | Debit returns Credit; Credit returns Debit
opposite :: DrCr -> DrCr
opposite d = case d of
  Debit -> Credit
  Credit -> Debit

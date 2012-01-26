module Penny.Bits.Entry where

import qualified Penny.Bits.Amount as A

data DrCr = Debit | Credit deriving Eq

data Entry = Entry { drCr :: DrCr
                   , amount :: A.Amount }



module Penny.Lincoln.Bits.Entry where

import Penny.Lincoln.Bits.Amount (Amount)
import Penny.Lincoln.Bits.DrCr (DrCr)

data Entry = Entry { drCr :: DrCr
                   , amount :: Amount }
             deriving (Eq, Show, Ord)



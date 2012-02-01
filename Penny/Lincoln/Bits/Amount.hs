module Penny.Lincoln.Bits.Amount where

import Penny.Lincoln.Bits.Qty ( Qty )
import Penny.Lincoln.Bits.Commodity ( Commodity )

data Amount = Amount { qty :: Qty
                     , commodity :: Commodity }
              deriving (Eq, Show, Ord)



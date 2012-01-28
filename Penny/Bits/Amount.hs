module Penny.Bits.Amount where

import qualified Penny.Bits.Qty as Q
import Penny.Bits.Commodity ( Commodity )

data Amount = Amount { qty :: Q.Qty
                     , commodity :: Commodity }
              deriving (Eq, Show, Ord)



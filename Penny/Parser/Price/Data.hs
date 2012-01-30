module Penny.Parser.Price.Data where

import Penny.Bits.Commodity ( Commodity )
import qualified Penny.Bits.PricePoint as PP
import Penny.Reports ( CommodityFmt )

data Data =
  Data { pricePoint :: PP.PricePoint
       , priceFormat :: (Commodity, CommodityFmt) }
  deriving Show


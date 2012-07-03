module Penny.Lincoln.Bits.PricePoint where

import Penny.Lincoln.Bits.Price (Price)
import Penny.Lincoln.Bits.DateTime (DateTime)

data PricePoint m = PricePoint { dateTime :: DateTime
                               , price :: Price
                               , ppMeta :: m }
                  deriving (Eq, Ord, Show)

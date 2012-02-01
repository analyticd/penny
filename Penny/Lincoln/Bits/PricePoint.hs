module Penny.Lincoln.Bits.PricePoint where

import Penny.Lincoln.Bits.Price (Price)
import Penny.Lincoln.Bits.DateTime (DateTime)

data PricePoint = PricePoint { dateTime :: DateTime
                             , price :: Price }
                  deriving (Eq, Ord, Show)

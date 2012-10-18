module Penny.Lincoln.Bits.PricePoint where

import Penny.Lincoln.Bits.Price (Price)
import Penny.Lincoln.Bits.DateTime (DateTime)
import qualified Penny.Lincoln.Meta as M

data PricePoint = PricePoint { dateTime :: DateTime
                             , price :: Price
                             , ppMeta :: M.PriceMeta }
                  deriving (Eq, Show)

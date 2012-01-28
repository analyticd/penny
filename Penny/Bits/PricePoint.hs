module Penny.Bits.PricePoint where

import qualified Penny.Bits.Price as P
import qualified Penny.Bits.DateTime as DT

data PricePoint = PricePoint { dateTime :: DT.DateTime
                             , price :: P.Price }
                  deriving (Eq, Ord, Show)

module Penny.Lincoln.Bits.DateTime where

import Data.Time ( UTCTime )

newtype DateTime = DateTime { unDateTime :: UTCTime }
                   deriving (Eq, Show, Ord)


module Penny.Lincoln.Trio where

import Penny.Lincoln.Decimal
import Penny.Lincoln.Common

data Trio
  = SZC Side NZGrouped Commodity Arrangement
  | SZ Side NZGrouped
  | SC Side Commodity
  | S Side
  | ZC NZGrouped Commodity Arrangement
  | Z NZGrouped
  | C Commodity
  | E
  deriving (Eq, Ord, Show)

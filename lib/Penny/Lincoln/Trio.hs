module Penny.Lincoln.Trio where

import Penny.Lincoln.Decimal
import Penny.Lincoln.Common

data Trio
  = SQC Side NZGrouped Commodity Arrangement
  | SQ Side NZGrouped
  | SC Side Commodity
  | S Side
  | QC NZGrouped Commodity Arrangement
  | Q NZGrouped
  | C Commodity
  | N
  deriving (Eq, Ord, Show)

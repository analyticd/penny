module Penny.Lincoln.Trio where

import Penny.Lincoln.Decimal
import Penny.Lincoln.Common

data Trio
  = SQC (Abstract Side) Commodity Arrangement
  | SQ (Abstract Side)
  | SC Side Commodity
  | S Side
  | QC Lessrad Commodity Arrangement
  | Q Lessrad
  | C Commodity
  | N
  deriving (Eq, Ord, Show)

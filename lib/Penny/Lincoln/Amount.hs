module Penny.Lincoln.Amount where

import Penny.Lincoln.Qty
import Penny.Lincoln.Commodity

data Amount = Amount Commodity Qty
  deriving (Eq, Ord, Show)

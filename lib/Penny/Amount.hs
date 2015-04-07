module Penny.Amount where

import Penny.Lincoln.Qty
import Penny.Commodity

data Amount = Amount Commodity Qty
  deriving (Eq, Ord, Show)

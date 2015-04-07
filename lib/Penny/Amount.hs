module Penny.Amount where

import Penny.Qty
import Penny.Commodity

data Amount = Amount Commodity Qty
  deriving (Eq, Ord, Show)

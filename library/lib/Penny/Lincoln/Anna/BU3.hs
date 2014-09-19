module Penny.Lincoln.Anna.BU3 where

import qualified Penny.Zenod as Zenod
import qualified Penny.Lincoln.Anna as NovDecs

data T
  = Zeroes Zenod.T
  | NoZeroes NovDecs.T
  deriving (Eq, Ord, Show)

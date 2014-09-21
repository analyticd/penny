module Penny.Core.Anna.BU3 where

import qualified Penny.Core.Anna.Zenod as Zenod
import qualified Penny.Core.Anna.NovDecs as NovDecs

data T
  = Zeroes Zenod.T
  | NoZeroes NovDecs.T
  deriving (Eq, Ord, Show)

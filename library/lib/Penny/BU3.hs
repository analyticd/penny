module Penny.BU3 where

import qualified Penny.Zenod as Zenod
import qualified Penny.NovDecs as NovDecs

data T
  = Zeroes Zenod.T
  | NoZeroes NovDecs.T
  deriving (Eq, Ord, Show)

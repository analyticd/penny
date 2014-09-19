module Penny.Lincoln.Anna.BG4 where

import qualified Penny.Zero as Zero
import qualified Penny.Radix as Radix
import qualified Penny.Lincoln.Anna.BG5 as BG5

data T r = T
  { leadZero :: Maybe Zero.T
  , radix :: Radix.T r
  , bg5 :: BG5.T r
  } deriving (Eq, Ord, Show)

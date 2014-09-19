module Penny.Lincoln.Radbu where

import qualified Penny.Lincoln.Anna.Radix as Radix
import qualified Penny.Lincoln.Anna.BU3 as BU3

data T r = T
  { radix :: Radix.T r
  , bu3 :: BU3.T
  } deriving (Eq, Ord, Show)

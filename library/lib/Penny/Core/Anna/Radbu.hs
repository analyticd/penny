module Penny.Core.Anna.Radbu where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.BU3 as BU3

data T r = T
  { radix :: Radix.T r
  , bu3 :: BU3.T
  } deriving (Eq, Ord, Show)

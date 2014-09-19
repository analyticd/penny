module Penny.Core.Anna.BG2 where

import qualified Penny.Core.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Core.Anna.Radix as Radix

data T r = T
  { radix :: Radix.T r
  , mayGroups :: Maybe (DecDecsMayGroups.T r)
  } deriving (Eq, Ord, Show)

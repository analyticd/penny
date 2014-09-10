module Penny.BG2 where

import qualified Penny.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Radix as Radix

data T r = T
  { radix :: Radix.T r
  , mayGroups :: Maybe (DecDecsMayGroups.T r)
  } deriving (Eq, Ord, Show)

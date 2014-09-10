module Penny.BG1 where

import qualified Penny.Radix as Radix
import qualified Penny.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.BG2 as BG2

data T r
  = GroupOnLeft r (DecDecsMayGroups.T r) (Maybe (BG2.T r))
  | GroupOnRight (Radix.T r) (DecDecsMayGroups.T r)
  deriving (Eq, Ord, Show)

module Penny.Core.Anna.BG1 where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Core.Anna.BG2 as BG2

data T r
  = GroupOnLeft r (DecDecsMayGroups.T r) (Maybe (BG2.T r))
  | GroupOnRight (Radix.T r) (DecDecsMayGroups.T r)
  deriving (Eq, Ord, Show)

module Penny.Lincoln.Anna.BG1 where

import qualified Penny.Radix as Radix
import qualified Penny.Lincoln.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Lincoln.Anna.BG2 as BG2

data T r
  = GroupOnLeft r (DecDecsMayGroups.T r) (Maybe (BG2.T r))
  | GroupOnRight (Radix.T r) (DecDecsMayGroups.T r)
  deriving (Eq, Ord, Show)

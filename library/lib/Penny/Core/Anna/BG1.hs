module Penny.Core.Anna.BG1 where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Core.Anna.BG2 as BG2
import qualified Penny.Core.DecDecs as DecDecs
import qualified Penny.Core.Anna.DecsGroup as DecsGroup
import qualified Penny.Core.Anna.SeqDecs as SeqDecs

data T r
  = GroupOnLeft r (DecDecsMayGroups.T r) (Maybe (BG2.T r))
  | GroupOnRight (Radix.T r) DecDecs.T (DecsGroup.T r) (SeqDecs.T r)
  deriving (Eq, Ord, Show)

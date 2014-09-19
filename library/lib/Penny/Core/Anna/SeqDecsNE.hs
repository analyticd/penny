module Penny.Core.Anna.SeqDecsNE where

import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.Anna.DecsGroup as DecsGroup

data T r = T
  { group1 :: DecsGroup.T r
  , groupsRest :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

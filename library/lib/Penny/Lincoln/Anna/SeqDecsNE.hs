module Penny.Lincoln.Anna.SeqDecsNE where

import qualified Penny.Lincoln.Anna.SeqDecs as SeqDecs
import qualified Penny.Lincoln.Anna.DecsGroup as DecsGroup

data T r = T
  { group1 :: DecsGroup.T r
  , groupsRest :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

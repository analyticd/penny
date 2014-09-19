module Penny.SeqDecsNE where

import qualified Penny.SeqDecs as SeqDecs
import qualified Penny.Lincoln.Anna.DecsGroup as DecsGroup

data T r = T
  { group1 :: DecsGroup.T r
  , groupsRest :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

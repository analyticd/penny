module Penny.Lincoln.Anna.DecDecsMayGroups where

import qualified Penny.Lincoln.Anna.DecDecs as DecDecs
import qualified Penny.Lincoln.Anna.SeqDecs as SeqDecs

data T r = T
  { first :: DecDecs.T
  , rest :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

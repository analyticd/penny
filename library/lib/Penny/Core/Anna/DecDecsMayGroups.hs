module Penny.Core.Anna.DecDecsMayGroups where

import qualified Penny.Core.DecDecs as DecDecs
import qualified Penny.Core.Anna.SeqDecs as SeqDecs

data T r = T
  { first :: DecDecs.T
  , rest :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

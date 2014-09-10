module Penny.DecDecsMayGroups where

import qualified Penny.DecDecs as DecDecs
import qualified Penny.SeqDecs as SeqDecs

data T r = T
  { first :: DecDecs.T
  , rest :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

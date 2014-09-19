module Penny.Core.Anna.DecsGroup where

import qualified Penny.Core.Anna.DecDecs as DecDecs

data T r = T
  { grouper :: r
  , decDecs :: DecDecs.T
  } deriving (Eq, Ord, Show)

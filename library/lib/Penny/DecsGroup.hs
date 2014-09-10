module Penny.DecsGroup where

import qualified Penny.DecDecs as DecDecs

data T r = T
  { grouper :: r
  , decDecs :: DecDecs.T
  } deriving (Eq, Ord, Show)

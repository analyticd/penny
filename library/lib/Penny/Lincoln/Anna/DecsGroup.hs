module Penny.Lincoln.Anna.DecsGroup where

import qualified Penny.Lincoln.Anna.DecDecs as DecDecs

data T r = T
  { grouper :: r
  , decDecs :: DecDecs.T
  } deriving (Eq, Ord, Show)

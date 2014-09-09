module Penny.Nodbu where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.BU1 as BU1

data T r = T
  { novDecs :: NovDecs.T
  , bu1 :: BU1.T r
  } deriving (Eq, Ord, Show)

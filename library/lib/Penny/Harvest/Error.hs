module Penny.Harvest.Error where

import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Harvest.Error.Detail as Detail

data T = T
  { clxn :: Clxn.T
  , detail :: Detail.T
  } deriving Show

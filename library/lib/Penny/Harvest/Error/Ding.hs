module Penny.Harvest.Error.Ding where

import qualified Penny.Core.Clxn as Clxn

data T = T
  { clxn :: Clxn.T
  , detail :: Detail.T
  } deriving (Eq, Ord, Show)

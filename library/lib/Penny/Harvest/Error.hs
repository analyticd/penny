module Penny.Harvest.Error where

import qualified Penny.Harvest.Error.Ding as Ding

data T = T
  { first :: Ding.T
  , rest :: Seq Ding.T
  } deriving (Eq, Ord, Show)

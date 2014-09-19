module Penny.Lincoln.Anna.RadZ where

import qualified Penny.Lincoln.Anna.Radix as Radix
import qualified Penny.Lincoln.Anna.Zeroes as Zeroes

data T r = T
  { radix :: Radix.T r
  , zeroes :: Zeroes.T
  } deriving (Eq, Ord, Show)

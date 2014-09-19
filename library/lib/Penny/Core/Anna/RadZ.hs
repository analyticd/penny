module Penny.Core.Anna.RadZ where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.Zeroes as Zeroes

data T r = T
  { radix :: Radix.T r
  , zeroes :: Zeroes.T
  } deriving (Eq, Ord, Show)

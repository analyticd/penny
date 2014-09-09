module Penny.RadZ where

import qualified Penny.Radix as Radix
import qualified Penny.Zeroes as Zeroes

data T r = T
  { radix :: Radix.T r
  , zeroes :: Zeroes.T
  } deriving (Eq, Ord, Show)

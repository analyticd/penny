module Penny.Zerabu where

import qualified Penny.Radix as Radix
import qualified Penny.BU3 as BU3
import qualified Penny.Zero as Zero

data T r = T
  { zero :: Zero.T
  , radix :: Radix.T r
  , bu3 :: BU3.T
  } deriving (Eq, Ord, Show)

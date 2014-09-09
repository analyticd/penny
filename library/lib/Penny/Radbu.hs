module Penny.Radbu where

import qualified Penny.Radix as Radix
import qualified Penny.BU3 as BU3

data T r = T
  { radix :: Radix.T r
  , bu3 :: BU3.T
  } deriving (Eq, Ord, Show)

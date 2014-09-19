module Penny.Lincoln.Anna.Zerabu where

import qualified Penny.Lincoln.Anna.Radix as Radix
import qualified Penny.Lincoln.Anna.BU3 as BU3
import qualified Penny.Lincoln.Anna.Zero as Zero

data T r = T
  { zero :: Zero.T
  , radix :: Radix.T r
  , bu3 :: BU3.T
  } deriving (Eq, Ord, Show)

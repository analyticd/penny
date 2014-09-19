module Penny.Core.Anna.Radun where

import qualified Penny.Core.Anna.Radix as Rad
import qualified Penny.Core.Anna.Zeroes as Zeroes

data T r = T
  { radix :: Rad.T r
  , mayZeroes :: Maybe Zeroes.T
  } deriving (Eq, Ord, Show)

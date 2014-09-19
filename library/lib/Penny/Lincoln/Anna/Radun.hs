module Penny.Lincoln.Anna.Radun where

import qualified Penny.Lincoln.Anna.Radix as Rad
import qualified Penny.Lincoln.Anna.Zeroes as Zeroes

data T r = T
  { radix :: Rad.T r
  , mayZeroes :: Maybe Zeroes.T
  } deriving (Eq, Ord, Show)

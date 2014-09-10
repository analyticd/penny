module Penny.Radun where

import qualified Penny.Radix as Rad
import qualified Penny.Zeroes as Zeroes

data T r = T
  { radix :: Rad.T r
  , mayZeroes :: Maybe Zeroes.T
  } deriving (Eq, Ord, Show)

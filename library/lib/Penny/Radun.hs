module Penny.Radun where

import qualified Penny.Radix as Rad
import qualified Penny.NU2 as NU2

data T r = T
  { radix :: Rad.T r
  , nu2 :: NU2.T
  } deriving (Eq, Ord, Show)

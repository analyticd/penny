module Penny.Znu1 where

import qualified Penny.Zero as Zero
import qualified Penny.NU1 as NU1

data T r = T
  { zero :: Zero.T
  , nu1 :: NU1.T r
  } deriving (Eq, Ord, Show)

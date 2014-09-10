module Penny.Znu1 where

import qualified Penny.Zero as Zero
import qualified Penny.Radun as Radun

data T r = T
  { zero :: Zero.T
  , mayRadun :: Maybe (Radun.T r)
  } deriving (Eq, Ord, Show)

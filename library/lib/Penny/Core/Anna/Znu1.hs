module Penny.Core.Anna.Znu1 where

import qualified Penny.Core.Anna.Zero as Zero
import qualified Penny.Core.Anna.Radun as Radun

data T r = T
  { zero :: Zero.T
  , mayRadun :: Maybe (Radun.T r)
  } deriving (Eq, Ord, Show)

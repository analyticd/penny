module Penny.Lincoln.Anna.Znu1 where

import qualified Penny.Lincoln.Anna.Zero as Zero
import qualified Penny.Lincoln.Anna.Radun as Radun

data T r = T
  { zero :: Zero.T
  , mayRadun :: Maybe (Radun.T r)
  } deriving (Eq, Ord, Show)

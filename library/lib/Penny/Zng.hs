module Penny.Zng where

import qualified Penny.Zero as Zero
import qualified Penny.Lincoln.Anna.NG1 as NG1

data T r = T
  { zero :: Zero.T
  , ng1 :: NG1.T r
  } deriving (Eq, Ord, Show)

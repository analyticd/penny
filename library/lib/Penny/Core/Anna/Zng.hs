module Penny.Core.Anna.Zng where

import qualified Penny.Core.Anna.Zero as Zero
import qualified Penny.Core.Anna.NG1 as NG1

data T r = T
  { zero :: Zero.T
  , ng1 :: NG1.T r
  } deriving (Eq, Ord, Show)

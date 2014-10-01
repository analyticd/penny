module Penny.Core.Anna.BU2 where

import qualified Penny.Core.Anna.Zero as Zero
import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.BU3 as BU3

data T r = T (Maybe Zero.T) (Radix.T r) BU3.T
  deriving (Eq, Ord, Show)

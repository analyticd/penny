module Penny.Tree.LZ6.Collector where

import qualified Penny.Tree.LZ6.Zero as Zero
import qualified Penny.Core.Anna.Zeroes as Zeroes

data T a = T [(Zeroes.T, a)] (Zero.T a)
  deriving (Eq, Ord, Show)


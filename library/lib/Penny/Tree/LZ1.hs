module Penny.Tree.LZ1 where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Tree.LZ2 as LZ2

data T a
  = T (Radix.T a) (Maybe (LZ2.T a))
  deriving (Eq, Ord, Show)

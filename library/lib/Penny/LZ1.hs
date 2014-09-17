module Penny.LZ1 where

import qualified Penny.Radix as Radix
import qualified Penny.LZ2 as LZ2

data T a
  = T (Radix.T a) (Maybe (LZ2.T a))
  deriving (Eq, Ord, Show)

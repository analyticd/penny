module Penny.Copper.LZ1 where

import qualified Penny.Lincoln.Anna.Radix as Radix
import qualified Penny.Copper.LZ2 as LZ2

data T a
  = T (Radix.T a) (Maybe (LZ2.T a))
  deriving (Eq, Ord, Show)

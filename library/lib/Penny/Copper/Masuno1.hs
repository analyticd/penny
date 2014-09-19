module Penny.Copper.Masuno1 where

import qualified Penny.Lincoln.Anna.Radix as Radix
import qualified Penny.Copper.Masuno1Radix1 as Masuno1Radix1

data T a
  = T (Radix.T a) (Maybe (Masuno1Radix1.T a))
  deriving (Eq, Ord, Show)

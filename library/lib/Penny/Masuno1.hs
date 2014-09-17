module Penny.Masuno1 where

import qualified Penny.Radix as Radix
import qualified Penny.Masuno1Radix1 as Masuno1Radix1

data T a
  = T (Radix.T a) (Maybe (Masuno1Radix1.T a))
  deriving (Eq, Ord, Show)

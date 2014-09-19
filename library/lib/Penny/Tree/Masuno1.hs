module Penny.Tree.Masuno1 where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Tree.Masuno1Radix1 as Masuno1Radix1

data T a
  = T (Radix.T a) (Maybe (Masuno1Radix1.T a))
  deriving (Eq, Ord, Show)

module Penny.Core.Anna.Radem where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.Decems as Decems

data T r = T
  { radix :: Radix.T r
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

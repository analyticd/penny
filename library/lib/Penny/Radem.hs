module Penny.Radem where

import qualified Penny.Radix as Radix
import qualified Penny.Decems as Decems

data T r = T
  { radix :: Radix.T r
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

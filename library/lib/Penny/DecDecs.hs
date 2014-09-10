module Penny.DecDecs where

import qualified Penny.Decems as Decems
import Deka.Native

data T = T
  { decem :: Decem
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

module Penny.Lincoln.Anna.DecDecs where

import qualified Penny.Lincoln.Anna.Decems as Decems
import Deka.Native

data T = T
  { decem :: Decem
  , decems :: Decems.T
  } deriving (Eq, Ord, Show)

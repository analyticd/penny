module Penny.Core.NovSign where

import Deka.Dec
import qualified Penny.Core.NovDecs as NovDecs

data T = T
  { novDecs :: NovDecs.T
  , sign :: Sign
  } deriving (Eq, Ord, Show)

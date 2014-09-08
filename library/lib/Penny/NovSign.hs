module Penny.NovSign where

import Deka.Dec
import qualified Penny.NovDecs as NovDecs

data T = T
  { novDecs :: NovDecs.T
  , sign :: Sign
  } deriving (Eq, Ord, Show)

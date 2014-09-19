module Penny.Lincoln.NovSign where

import Deka.Dec
import qualified Penny.Lincoln.Anna as NovDecs

data T = T
  { novDecs :: NovDecs.T
  , sign :: Sign
  } deriving (Eq, Ord, Show)

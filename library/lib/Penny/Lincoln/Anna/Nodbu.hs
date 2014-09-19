module Penny.Lincoln.Anna.Nodbu where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Radem as Radem

data T r = T
  { novDecs :: NovDecs.T
  , mayRadem :: Maybe (Radem.T r)
  } deriving (Eq, Ord, Show)

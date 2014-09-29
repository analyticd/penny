module Penny.Core.Anna.Nodbu where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Core.Anna.Radem as Radem

data T r = T
  { novDecs :: NovDecs.T
  , mayRadem :: Maybe (Radem.T r)
  } deriving (Eq, Ord, Show)

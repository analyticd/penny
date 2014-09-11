module Penny.Nodbu where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.Radem as Radem

data T r = T
  { novDecs :: NovDecs.T
  , mayRadem :: Maybe (Radem.T r)
  } deriving (Eq, Ord, Show)

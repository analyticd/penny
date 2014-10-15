module Penny.Core.NovSign where

import qualified Penny.Core.Sign as Sign
import qualified Penny.Core.NovDecs as NovDecs

data T = T
  { novDecs :: NovDecs.T
  , sign :: Sign.T
  } deriving (Eq, Ord, Show)

module Penny.Core.CoeffExp where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Core.Exp as Exp

data T = T
  { coefficient :: NovDecs.T
  , exp :: Exp.T
  } deriving (Eq, Ord, Show)

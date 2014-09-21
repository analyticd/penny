module Penny.Core.Anna.Zenod where

import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Core.Anna.NovDecs as NovDecs

data T = T
  { zeroes :: Zeroes.T
  , novDecs :: NovDecs.T
  } deriving (Eq, Ord, Show)

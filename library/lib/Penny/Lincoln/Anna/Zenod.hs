module Penny.Lincoln.Anna.Zenod where

import qualified Penny.Lincoln.Anna.Zeroes as Zeroes
import qualified Penny.Lincoln.Anna as NovDecs

data T = T
  { zeroes :: Zeroes.T
  , novDecs :: NovDecs.T
  } deriving (Eq, Ord, Show)

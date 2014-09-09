module Penny.Zenod where

import qualified Penny.Zeroes as Zeroes
import qualified Penny.NovDecs as NovDecs

data T = T
  { zeroes :: Zeroes.T
  , novDecs :: NovDecs.T
  } deriving (Eq, Ord, Show)

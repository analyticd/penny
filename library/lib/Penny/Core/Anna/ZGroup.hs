module Penny.Core.Anna.ZGroup where

import qualified Penny.Core.Anna.Zeroes as Zeroes

data T g = T
  { grouper :: g
  , zeroes :: Zeroes.T
  } deriving (Eq, Ord, Show)

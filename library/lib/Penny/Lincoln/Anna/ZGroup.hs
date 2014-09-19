module Penny.Lincoln.Anna.ZGroup where

import qualified Penny.Lincoln.Anna.Zeroes as Zeroes

data T g = T
  { grouper :: g
  , zeroes :: Zeroes.T
  } deriving (Eq, Ord, Show)

module Penny.ZGroup where

import qualified Penny.Zeroes as Zeroes

data T g = T
  { grouper :: g
  , zeroes :: Zeroes.T
  } deriving (Eq, Ord, Show)

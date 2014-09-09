module Penny.NU2 where

import qualified Penny.Zeroes as Zeroes

data T
  = E
  | F Zeroes.T
  deriving (Eq, Ord, Show)

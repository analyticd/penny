module Penny.BU1 where

import qualified Penny.Radem as Radem

data T r
  = E
  | F (Radem.T r)
  deriving (Eq, Ord, Show)

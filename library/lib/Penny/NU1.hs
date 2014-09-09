module Penny.NU1 where

import qualified Penny.Radun as Radun

data T r
  = E
  | F (Radun.T r)
  deriving (Eq, Ord, Show)

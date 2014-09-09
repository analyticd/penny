module Penny.BU where

import qualified Penny.Nodbu as Nodbu
import qualified Penny.BU2 as BU2

data T r
  = Masuno (Nodbu.T r)
  | Fracuno (BU2.T r)
  deriving (Eq, Ord, Show)

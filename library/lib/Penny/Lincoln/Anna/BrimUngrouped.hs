module Penny.Lincoln.Anna.BrimUngrouped where

import qualified Penny.Lincoln.Anna.Nodbu as Nodbu
import qualified Penny.Lincoln.Anna.BU2 as BU2

data T r
  = Masuno (Nodbu.T r)
  | Fracuno (BU2.T r)
  deriving (Eq, Ord, Show)

module Penny.Core.Anna.BrimUngrouped where

import qualified Penny.Core.Anna.Nodbu as Nodbu
import qualified Penny.Core.Anna.BU2 as BU2

data T r
  = Masuno (Nodbu.T r)
  | Fracuno (BU2.T r)
  deriving (Eq, Ord, Show)

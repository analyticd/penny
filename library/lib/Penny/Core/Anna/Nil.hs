module Penny.Core.Anna.Nil where

import qualified Penny.Core.Anna.Nil.Ungrouped as NU
import qualified Penny.Core.Anna.Nil.Grouped as NG

data T r
  = Ungrouped (NU.T r)
  | Grouped (NG.T r)
  deriving (Eq, Ord, Show)

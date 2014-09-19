module Penny.Lincoln.Anna.Nil where

import qualified Penny.Lincoln.Anna.Nil.Ungrouped as NU
import qualified Penny.Lincoln.Anna.Nil.Grouped as NG

data T r
  = Ungrouped (NU.T r)
  | Grouped (NG.T r)
  deriving (Eq, Ord, Show)

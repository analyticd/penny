module Penny.Nil where

import qualified Penny.NilUngrouped as NU
import qualified Penny.NilGrouped as NG

data T r
  = Ungrouped (NU.T r)
  | Grouped (NG.T r)
  deriving (Eq, Ord, Show)

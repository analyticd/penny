module Penny.Nil where

import qualified Penny.NU as NU
import qualified Penny.NG as NG

data T r
  = Ungrouped (NU.T r)
  | Grouped (NG.T r)
  deriving (Eq, Ord, Show)

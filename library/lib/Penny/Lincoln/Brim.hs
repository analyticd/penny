module Penny.Lincoln.Brim where

import qualified Penny.Lincoln.Anna.BrimGrouped as BG
import qualified Penny.Lincoln.Anna.BrimUngrouped as BU

data T r
  = Grouped (BG.T r)
  | Ungrouped (BU.T r)
  deriving (Eq, Ord, Show)

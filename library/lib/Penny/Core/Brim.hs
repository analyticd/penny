module Penny.Core.Brim where

import qualified Penny.Core.Anna.BrimGrouped as BG
import qualified Penny.Core.Anna.BrimUngrouped as BU

data T r
  = Grouped (BG.T r)
  | Ungrouped (BU.T r)
  deriving (Eq, Ord, Show)

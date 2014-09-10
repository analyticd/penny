module Penny.Brim where

import qualified Penny.BrimGrouped as BG
import qualified Penny.BrimUngrouped as BU

data T r
  = Grouped (BG.T r)
  | Ungrouped (BU.T r)
  deriving (Eq, Ord, Show)

module Penny.Core.Anna.Brim where

import qualified Penny.Core.Anna.BrimGrouped as BG
import qualified Penny.Core.Anna.BrimUngrouped as BU
import qualified Penny.Core.Anna.BG1 as BG1
import qualified Penny.Core.Gravel as Gravel

data T r
  = Grouped (BG.T r)
  | Ungrouped (BU.T r)
  deriving (Eq, Ord, Show)

toGravel :: T r -> Gravel.T a
toGravel (Grouped (BG.Masuno novDecs (BG1.GroupOnLeft _
  ddMayGrps Nothing))) = undefined

-- | BrimGrouped

module Penny.Core.Anna.BrimGrouped where

import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.BG1 as BG1
import qualified Penny.Core.Anna.BG4 as BG4

data T r
  = Masuno NovDecs.T (BG1.T r)
  | Fraucno (BG4.T r)
  deriving (Eq, Ord, Show)

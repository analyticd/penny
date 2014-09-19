-- | BrimGrouped

module Penny.Lincoln.Anna.BrimGrouped where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Lincoln.Anna.BG1 as BG1
import qualified Penny.Lincoln.Anna.BG4 as BG4

data T r
  = Masuno NovDecs.T (BG1.T r)
  | Fraucno (BG4.T r)
  deriving (Eq, Ord, Show)

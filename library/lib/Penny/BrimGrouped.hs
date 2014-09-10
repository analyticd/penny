-- | BrimGrouped

module Penny.BrimGrouped where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.BG1 as BG1
import qualified Penny.BG4 as BG4

data T r
  = Masuno NovDecs.T (BG1.T r)
  | Fraucno (BG4.T r)
  deriving (Eq, Ord, Show)

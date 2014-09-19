-- | NilGrouped
module Penny.Core.Anna.Nil.Grouped where

import qualified Penny.Core.Anna.Zng as Zng
import qualified Penny.Core.Anna.NG1 as NG1

data T r
  = LeadingZero (Zng.T r)
  | NoLeadingZero (NG1.T r)
  deriving (Eq, Ord, Show)

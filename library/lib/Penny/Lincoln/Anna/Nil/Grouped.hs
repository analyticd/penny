-- | NilGrouped
module Penny.Lincoln.Anna.Nil.Grouped where

import qualified Penny.Lincoln.Anna.Zng as Zng
import qualified Penny.Lincoln.Anna.NG1 as NG1

data T r
  = LeadingZero (Zng.T r)
  | NoLeadingZero (NG1.T r)
  deriving (Eq, Ord, Show)

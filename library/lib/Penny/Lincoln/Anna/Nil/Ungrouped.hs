module Penny.Lincoln.Anna.Nil.Ungrouped where

import qualified Penny.Lincoln.Anna.Znu1 as Znu1
import qualified Penny.Lincoln.Anna.RadZ as RadZ

data T r
  = LeadingZero (Znu1.T r)
  | NoLeadingZero (RadZ.T r)
  deriving (Eq, Ord, Show)

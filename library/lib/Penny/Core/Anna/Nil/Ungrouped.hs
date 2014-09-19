module Penny.Core.Anna.Nil.Ungrouped where

import qualified Penny.Core.Anna.Znu1 as Znu1
import qualified Penny.Core.Anna.RadZ as RadZ

data T r
  = LeadingZero (Znu1.T r)
  | NoLeadingZero (RadZ.T r)
  deriving (Eq, Ord, Show)

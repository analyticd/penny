module Penny.NU where

import qualified Penny.Znu1 as Znu1
import qualified Penny.RadZ as RadZ

data T r
  = LeadingZero (Znu1.T r)
  | NoLeadingZero (RadZ.T r)
  deriving (Eq, Ord, Show)

module Penny.Lincoln.Anna.BU2 where

import qualified Penny.Lincoln.Anna.Zerabu as Zerabu
import qualified Penny.Lincoln.Radbu as Radbu

data T r
  = LeadingZero (Zerabu.T r)
  | NoLeadingZero (Radbu.T r)
  deriving (Eq, Ord, Show)

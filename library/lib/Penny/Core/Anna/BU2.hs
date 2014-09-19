module Penny.Core.Anna.BU2 where

import qualified Penny.Core.Anna.Zerabu as Zerabu
import qualified Penny.Core.Radbu as Radbu

data T r
  = LeadingZero (Zerabu.T r)
  | NoLeadingZero (Radbu.T r)
  deriving (Eq, Ord, Show)

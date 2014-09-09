module Penny.BU2 where

import qualified Penny.Zerabu as Zerabu
import qualified Penny.Radbu as Radbu

data T r
  = LeadingZero (Zerabu.T r)
  | NoLeadingZero (Radbu.T r)
  deriving (Eq, Ord, Show)

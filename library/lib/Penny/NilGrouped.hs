-- | NilGrouped
module Penny.NilGrouped where

import qualified Penny.Zng as Zng
import qualified Penny.NG1 as NG1

data T r
  = LeadingZero (Zng.T r)
  | NoLeadingZero (NG1.T r)
  deriving (Eq, Ord, Show)

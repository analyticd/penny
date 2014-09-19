-- | An Ingot is a Lewis paired with an optional Currency.

module Penny.Tree.Ingot where

import qualified Penny.Tree.Ingot.Period as Period
import qualified Penny.Tree.Ingot.Comma as Comma

data T
  = Comma Comma.T
  | Period Period.T
  deriving (Eq, Ord, Show)

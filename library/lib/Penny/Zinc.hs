module Penny.Zinc where

import qualified Penny.Legend as Lincoln
import qualified Penny.Lincoln.Side as Side

newtype T r
  = T { toLincoln :: Lincoln.T r Side.T }
  deriving (Eq, Ord, Show)

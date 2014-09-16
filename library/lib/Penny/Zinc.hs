module Penny.Zinc where

import qualified Penny.Lincoln as Lincoln
import qualified Penny.Side as Side

newtype T r
  = T { toLincoln :: Lincoln.T r Side.T }
  deriving (Eq, Ord, Show)

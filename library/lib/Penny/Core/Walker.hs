module Penny.Core.Walker where

import qualified Penny.Core.Stokely as Lincoln
import qualified Penny.Core.Side as Side

newtype T r
  = T { toLincoln :: Lincoln.T r Side.T }
  deriving (Eq, Ord, Show)

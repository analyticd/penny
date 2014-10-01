module Penny.Core.Trio.Error where

import qualified Penny.Core.Commodity as Cy
import qualified Penny.Core.Quark as Quark
import qualified Penny.Core.Side as Side
import qualified Penny.Core.Philly as Philly

data T
  = NoImbalances
  | MultipleImbalances (Cy.T, Quark.T) (Cy.T, Quark.T) [(Cy.T, Quark.T)]
  | CommodityNotFound Cy.T
  | BalanceIsSameSide Side.T
  | UnsignedTooLarge Philly.T Quark.T
  deriving (Eq, Ord, Show)

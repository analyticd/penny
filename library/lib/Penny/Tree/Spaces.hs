module Penny.Tree.Spaces where

import qualified Penny.Natural.NonZero as NonZero

newtype T = T { toNonZero :: NonZero.T }
  deriving (Eq, Ord, Show)

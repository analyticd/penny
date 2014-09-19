module Penny.Lincoln.Anna.Zeroes where

import qualified Penny.Natural.NonZero as NonZero

-- | One or more zeroes.
newtype T = T { toNonZero :: NonZero.T }
  deriving (Eq, Ord, Show)

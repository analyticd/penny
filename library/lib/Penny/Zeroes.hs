module Penny.Zeroes where

import qualified Penny.NonZero as NonZero

-- | One or more zeroes.
newtype T = T { toNonZero :: NonZero.T }
  deriving (Eq, Ord, Show)

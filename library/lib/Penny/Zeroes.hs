module Penny.Zeroes where

import qualified Penny.NonZero as NonZero

newtype T = T { toNonZero :: NonZero.T }
  deriving (Eq, Ord, Show)

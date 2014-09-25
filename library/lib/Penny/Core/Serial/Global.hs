module Penny.Core.Serial.Global where

import qualified Penny.Core.Serial as Serial

newtype T = T { toSerial :: Serial.T }
  deriving (Eq, Ord, Show)

-- after Timothy Thomas Fortune

module Penny.Core.Fortune where

import qualified Penny.Core.Bundle as Bundle
import qualified Penny.Core.Serial as Serial

-- | A 'Penny.Core.Bundle.T', combined with an additional serial
-- indicating the order in which this posting appears after filtering.

data T = T
  { bundle :: Bundle.T
  , postMainFilter :: Serial.T
  -- ^ After postings are filtered to determine which postings will be
  -- part of a report, they are assigned this serial.
  } deriving (Eq, Ord, Show)


module Penny.Amount.Period where

import qualified Penny.Amount as Amount
import qualified Penny.Lincoln.Anna.RadPer as RadPer

-- | An Amount with a period as a radix.

data T = T { toAmount :: Amount.T RadPer.T }
  deriving (Eq, Ord, Show)

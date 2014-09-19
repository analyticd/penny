module Penny.Amount.Pre where

import qualified Penny.Char.Currency as Currency
import qualified Penny.Copper.Lewis as Wheat

-- | A parse tree where the currency is on the left, followed by a
-- representation.

data T a = T Currency.T (Maybe (Wheat.T a))
  deriving (Eq, Ord, Show)

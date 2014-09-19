module Penny.Amount.Post where

import qualified Penny.Copper.Currency as Currency
import qualified Penny.Copper.Lewis as Wheat

-- | A parse tree where the representation is on the right, followed
-- by an optional commodity.

data T a = T (Wheat.T a) (Maybe Currency.T)
  deriving (Eq, Ord, Show)

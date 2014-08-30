module Penny.Pricing.Price where

import Penny.Pricing.Trade
import Penny.Common
import Penny.Numbers.Abstract.Signed
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Exchange
import Penny.DateTime

data Price = Price
  { priTrade :: Trade
  , priExchange :: Either (Signed Period PluMin) (Signed Comma PluMin)
  , priDate :: DateTime
  , priArrangement :: Arrangement
  , priLocation :: Location
  , priClxn :: Clxn
  } deriving (Eq, Ord, Show)


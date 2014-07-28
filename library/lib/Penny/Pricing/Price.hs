module Penny.Pricing.Price where

import Penny.Pricing.Trade
import Penny.Common
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Exchange
import Penny.DateTime

data Price = Price
  { pTrade :: Trade
  , pExchange :: Either (Polar Period PluMin) (Polar Comma PluMin)
  } deriving (Eq, Ord, Show)

data PricePoint = PricePoint
  { price :: Price
  , priceDate :: DateTime
  } deriving (Eq, Ord, Show)

data PriceMeta = PriceMeta
  { arrangement :: Arrangement
  , line :: Line
  , filename :: Filename
  } deriving (Eq, Ord, Show)

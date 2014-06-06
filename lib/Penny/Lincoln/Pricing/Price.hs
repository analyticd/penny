module Penny.Lincoln.Pricing.Price where

import Penny.Lincoln.Pricing.Trade
import Penny.Lincoln.Common
import Penny.Lincoln.Decimal

data Price a = Price
  { pTrade :: Trade
  , pExchange :: a
  } deriving (Eq, Ord, Show)

instance Functor Price where
  fmap f (Price t a) = Price t (f a)

data PricePoint a = PricePoint
  { price :: Price a
  , priceDate :: DateTime
  } deriving (Eq, Ord, Show)

instance Functor PricePoint where
  fmap f (PricePoint p dt) = PricePoint (fmap f p) dt

data PriceMeta = PriceMeta
  { arrangement :: Arrangement
  , line :: Line
  , filename :: Filename
  } deriving (Eq, Ord, Show)

newtype ExchangeA = ExchangeA { unExchangeA :: Abstract PosNeg }
  deriving (Eq, Ord, Show)


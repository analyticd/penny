{-# LANGUAGE NoImplicitPrelude #-}
module Penny.Pricing.Price.Shrinkers where

import Penny.Pricing.Trade.Shrinkers
import Prelude.Shrinkers
import Penny.Numbers.Abstract.Aggregates.Shrinkers
import Penny.Pricing.Price hiding (price, arrangement, line, filename)
import Penny.DateTime.Shrinkers
import Penny.Common.Shrinkers

price :: Price -> [Price]
price (Price t e) =
  [ Price t' e' | (t', e') <-
    tuple2 trade (either polar polar) (t, e) ]

pricePoint :: PricePoint -> [PricePoint]
pricePoint (PricePoint p d) =
  [ PricePoint p' d' | (p', d') <-
    tuple2 price dateTime (p, d) ]

priceMeta :: PriceMeta -> [PriceMeta]
priceMeta (PriceMeta a l f) =
  [ PriceMeta a' l' f' | (a', l', f') <-
    tuple3 arrangement line filename (a, l, f) ]

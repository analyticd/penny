{-# LANGUAGE NoImplicitPrelude #-}
module Penny.Pricing.Price.Generators where

import Penny.Pricing.Price hiding (price, arrangement, line, filename)
import Penny.Numbers.Abstract.RadGroup.Generators
import Penny.Numbers.Abstract.Aggregates.Generators
import Prelude.Generators
import Penny.Numbers.Exchange.Generators
import Test.QuickCheck
import Control.Monad
import Penny.Numbers.Abstract.RadGroup
import Penny.Pricing.Trade.Generators
import Penny.DateTime.Generators
import Penny.Common.Generators

price :: Gen Price
price = liftM2 Price trade
  (either (polar pluMin (return radPeriod) groupPeriod)
          (polar pluMin (return radComma) groupComma))

pricePoint :: Gen PricePoint
pricePoint = liftM2 PricePoint price dateTime

priceMeta :: Gen PriceMeta
priceMeta = liftM3 PriceMeta arrangement line filename

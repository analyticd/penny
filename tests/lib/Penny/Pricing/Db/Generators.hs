module Penny.Pricing.Db.Generators where

import Penny.Pricing.Trade.Generators
import Penny.Numbers.Exchange.Generators
import Data.Map.Generators
import Prelude hiding (map)
import Data.Time.Generators
import Control.Monad
import Data.Time
import Penny.Numbers.Exchange
import Test.QuickCheck
import Data.Map (Map)
import Penny.Pricing.Db
import Penny.Pricing.Trade hiding (to, from)

exchMap :: Gen (Map UTCTime Exch)
exchMap = map (listOf (liftM2 (,) uTCTime exch))

toMap :: Gen (Map To ExchMap)
toMap = map (listOf (liftM2 (,) to exchMap))

priceDb :: Gen PriceDb
priceDb = fmap PriceDb $ map (listOf (liftM2 (,) from toMap))

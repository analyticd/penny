module Penny.Lincoln.Pricing.Db where

import qualified Data.Map as M
import Data.Time
import Penny.Lincoln.Decimal
import Penny.Lincoln.Pricing.Trade

type ExchMap = M.Map UTCTime Exchange
type ToMap = M.Map To ExchMap

newtype PriceDb = PriceDb (M.Map From ToMap)

emptyDb :: PriceDb
emptyDb = PriceDb M.empty

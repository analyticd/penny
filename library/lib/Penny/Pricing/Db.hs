module Penny.Pricing.Db where

import qualified Data.Map as M
import Data.Time
import Penny.Numbers.Exchange
import Penny.Pricing.Trade

type ExchMap = M.Map UTCTime Exch
type ToMap = M.Map To ExchMap

newtype PriceDb = PriceDb (M.Map From ToMap)

emptyDb :: PriceDb
emptyDb = PriceDb M.empty

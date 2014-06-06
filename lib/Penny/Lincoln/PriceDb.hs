module Penny.Lincoln.PriceDb where

import qualified Data.Map as M
import Data.Time
import Penny.Lincoln.Bits.Open

{-
type CpuMap = M.Map UTCTime Multiplier
type ToMap = M.Map To CpuMap

newtype PriceDb = PriceDb (M.Map From ToMap)

emptyDb :: PriceDb
emptyDb = PriceDb M.empty


-}

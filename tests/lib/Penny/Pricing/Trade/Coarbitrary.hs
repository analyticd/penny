module Penny.Pricing.Trade.Coarbitrary where

import Penny.Pricing.Trade hiding (from, to, trade)
import Test.QuickCheck
import Penny.Common.Coarbitrary
import qualified Penny.Pricing.Trade as T

trade :: Trade -> Gen b -> Gen b
trade t = from (T.from t) . to (T.to t)

from :: From -> Gen b -> Gen b
from (From x) = commodity x

to :: To -> Gen b -> Gen b
to (To x) = commodity x

module Penny.Pricing.Trade.Shrinkers where

import Penny.Pricing.Trade hiding (trade, from, to)
import Penny.Common.Shrinkers

from :: From -> [From]
from = fmap From . commodity . unFrom

to :: To -> [To]
to = fmap To . commodity . unTo

trade :: Trade -> [Trade]
trade _ = []

module Penny.Pricing.Trade.Generators where

import Penny.Common.Generators
import Penny.Pricing.Trade hiding (trade, from, to)
import qualified Penny.Pricing.Trade as T
import Test.QuickCheck
import Control.Monad
import Data.Maybe

from :: Gen From
from = fmap From commodity

to :: Gen To
to = fmap To commodity

trade :: Gen Trade
trade = fmap f $ ((liftM2 (,) from to) `suchThat` pd)
  where
    pd (From fr, To t) = fr /= t
    f = fromMaybe (error "could not generate trade")
      . uncurry T.trade

module Penny.TopLine.Coarbitrary where

import Penny.TopLine
import Penny.Common.Coarbitrary
import Penny.DateTime.Coarbitrary
import Test.QuickCheck
import Penny.Serial.Coarbitrary
import Prelude.Coarbitrary
import Prelude hiding (maybe)

topLineData :: TopLineData -> Gen b -> Gen b
topLineData (TopLineData d m n f p) =
  dateTime d . memo m . number n . flag f . payee p

topLineMeta :: TopLineMeta -> Gen b -> Gen b
topLineMeta (TopLineMeta m l g f n) =
  line m . line l . serial g . serial f . filename n

topLine :: TopLine -> Gen b -> Gen b
topLine (TopLine d m) = topLineData d . maybe topLineMeta m

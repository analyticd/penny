module Penny.Lincoln.Decimal.Zero.Generators where

import Penny.Lincoln.Decimal.Zero
import Penny.Lincoln.Natural.Generators
import Test.QuickCheck
import Control.Monad

plainZero :: Gen PlainZero
plainZero = return PlainZero

group :: Gen Group
group = fmap Group positive

groupedZero :: Gen GroupedZero
groupedZero = liftM3 GroupedZero arbitrary group (listOf group)

zero :: Gen Zero
zero = fmap Zero $
  oneof [ fmap Left plainZero
        , fmap Right groupedZero ]

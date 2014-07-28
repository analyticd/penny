module Penny.Numbers.Abstract.RadGroup.Generators where

import Test.QuickCheck
import Penny.Numbers.Abstract.RadGroup

groupPeriod :: Gen (b -> Group Period b)
groupPeriod = elements
  [ comma, space, thin, under ]

groupComma :: Gen (b -> Group Comma b)
groupComma = elements
  [ period, space, thin, under ]

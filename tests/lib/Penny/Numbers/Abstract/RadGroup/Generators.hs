module Penny.Numbers.Abstract.RadGroup.Generators where

import Test.QuickCheck
import Penny.Numbers.Abstract.RadGroup

group
  :: Gen b
  -> Gen (b -> Group a b)
  -> Gen (Group a b)
group gb gf = do
  f <- gf
  b <- gb
  return $ f b

groupPeriod :: Gen (b -> Group Period b)
groupPeriod = elements
  [ comma, space, thin, under ]

groupComma :: Gen (b -> Group Comma b)
groupComma = elements
  [ period, space, thin, under ]

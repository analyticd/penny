module Penny.Numbers.Abstract.RadGroup.Generators where

import Test.QuickCheck
import Penny.Numbers.Abstract.RadGroup

group
  :: Gen b
  -> Gen (Grouper r)
  -> Gen (Group r b)
group gb gf = do
  b <- gb
  f <- gf
  return $ Group f b

grouperPeriod :: Gen (Grouper Period)
grouperPeriod = elements
  [ comma, space, thin, under ]

grouperComma :: Gen (Grouper Comma)
grouperComma = elements
  [ period, space, thin, under ]

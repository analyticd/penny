{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Penny.Lincoln.Properties where

import Penny.Lincoln
import Penny.Lincoln.Instances ()
import Test.Tasty.QuickCheck
import Test.Tasty.TH

prop_repUngroupedDecimal dec
  = toDecimal (repUngroupedDecimal (Radix :: Radix ()) dec)
  == dec

prop_repUngroupedDecNonZero dec
  = pos == dec
  where
    pos = c'DecNonZero'DecPositive pm . toDecPositive $ bu
    (bu, pm) = repUngroupedDecNonZero (Radix :: Radix ()) dec

testGroup = $(testGroupGenerator)

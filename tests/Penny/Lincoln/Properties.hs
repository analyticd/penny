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

prop_repUngroupedDecUnsigned dec
  = case (decomposeDecUnsigned dec, rep) of
    (Left z, Center nu) -> toDecZero nu == z
    (Right p, OffCenter bu ()) -> toDecPositive bu == p
    _ -> False
  where
    rep = repUngroupedDecUnsigned (Radix :: Radix ()) dec

prop_repUngroupedDecZero dec
  = toDecZero (repUngroupedDecZero (Radix :: Radix ()) dec)
  == dec

testGroup = $(testGroupGenerator)

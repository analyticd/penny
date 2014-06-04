module Penny.Lincoln.Decimal.Lane.Generators where

import Penny.Lincoln.Decimal.Lane
import Deka.Native.Abstract.Generators
import Test.QuickCheck
import Control.Monad

lane :: Gen a -> Gen (Lane a)
lane g = frequency [(1, return Center),
  (3, fmap NonCenter (liftM2 (,) g decuple))]

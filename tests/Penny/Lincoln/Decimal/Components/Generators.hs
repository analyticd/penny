module Penny.Lincoln.Decimal.Components.Generators where

import Penny.Lincoln.Natural.Generators
import Penny.Lincoln.Decimal.Components
import Prelude hiding (exponent)
import Test.QuickCheck

exponent :: Gen Exponent
exponent = fmap Exponent nonNegative

lane :: Gen a -> Gen (Lane a)
lane g = frequency [(1, return Center),
  (3, fmap NonCenter (liftM2 (,) g decuple))]

side :: Gen Side
side = elements [Debit, Credit]

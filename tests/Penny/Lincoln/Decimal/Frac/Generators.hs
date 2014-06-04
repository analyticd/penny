module Penny.Lincoln.Decimal.Frac.Generators where

import Test.QuickCheck
import Penny.Lincoln.Decimal.Frac
import Deka.Native.Abstract.Generators
import Penny.Lincoln.Natural.Generators
import Control.Monad

zeroes :: Gen Zeroes
zeroes = fmap Zeroes positive

msg :: Gen MSG
msg = liftM3 MSG nonNegative novem (listOf decem)

lsg :: Gen LSG
lsg = liftM2 LSG decem (listOf decem)

frac :: Gen Frac
frac = liftM4 Frac arbitrary (listOf zeroes) msg (listOf lsg)

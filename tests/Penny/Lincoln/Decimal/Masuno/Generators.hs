module Penny.Lincoln.Decimal.Masuno.Generators where

import Deka.Native.Abstract.Generators
import Penny.Lincoln.Decimal.Masuno
import Test.QuickCheck
import Control.Monad

msg :: Gen MSG
msg = liftM2 MSG novem (listOf decem)

lsg :: Gen LSG
lsg = liftM2 LSG decem (listOf decem)

fg :: Gen FG
fg = liftM2 FG decem (listOf decem)

monly :: Gen Monly
monly = liftM2 Monly msg (listOf lsg)

fracuno :: Gen Fracuno
fracuno = liftM3 Fracuno msg (listOf lsg) (listOf fg)

masuno :: Gen Masuno
masuno = fmap Masuno $ oneof [ fmap Left monly, fmap Right fracuno ]

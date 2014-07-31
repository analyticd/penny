module Penny.Numbers.Qty.Coarbitrary where

import qualified Penny.Numbers.Qty as Q
import Test.QuickCheck
import Penny.Numbers.Concrete.Coarbitrary
import Barecheck.Util

qty :: Q.Qty -> Gen b -> Gen b
qty (Q.Qty c) = concrete c

side :: Q.Side -> Gen b -> Gen b
side s = case s of
  Q.Debit -> varInt 0
  Q.Credit -> varInt 1

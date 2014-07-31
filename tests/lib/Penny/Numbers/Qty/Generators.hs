module Penny.Numbers.Qty.Generators where

import qualified Penny.Numbers.Qty as Q
import Test.QuickCheck
import Penny.Numbers.Concrete.Generators

qty :: Gen Q.Qty
qty = fmap Q.Qty concrete

side :: Gen Q.Side
side = elements [ Q.Debit, Q.Credit ]

module Penny.Lincoln.Decimal.Side.Generators where

import Test.QuickCheck
import Penny.Lincoln.Decimal.Side

side :: Gen Side
side = elements [Debit, Credit]

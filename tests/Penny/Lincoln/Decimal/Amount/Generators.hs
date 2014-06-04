module Penny.Lincoln.Decimal.Amount.Generators where

import Penny.Lincoln.Decimal.Amount
import Test.QuickCheck
import Penny.Lincoln.Decimal.Concrete.Generators
import Penny.Lincoln.Decimal.Abstract.Generators

amount :: Gen Amount
amount = oneof
  [ fmap AAbstract abstract
  , fmap AConcrete concrete
  ]

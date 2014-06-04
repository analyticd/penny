module Penny.Lincoln.Decimal.Components.Generators where

import Penny.Lincoln.Natural.Generators
import Penny.Lincoln.Decimal.Components
import Prelude hiding (exponent)
import Test.QuickCheck

exponent :: Gen Exponent
exponent = fmap Exponent nonNegative

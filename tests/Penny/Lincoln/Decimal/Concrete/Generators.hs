module Penny.Lincoln.Decimal.Concrete.Generators where

import Test.QuickCheck
import Penny.Lincoln.Decimal.Concrete hiding (concrete)
import qualified Penny.Lincoln.Decimal.Concrete as C
import Penny.Lincoln.Decimal.Abstract.Generators
import Penny.Lincoln.Decimal.Components.Generators

concrete :: Gen Concrete
concrete = fmap C.concrete (rep side)

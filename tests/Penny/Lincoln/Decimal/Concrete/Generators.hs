module Penny.Lincoln.Decimal.Concrete.Generators where

import Test.QuickCheck
import Penny.Lincoln.Decimal.Concrete hiding (concrete)
import qualified Penny.Lincoln.Decimal.Concrete as C
import Penny.Lincoln.Decimal.Abstract.Generators

concrete :: Gen Concrete
concrete = fmap C.concrete rep

add :: Gen Add
add = fmap Add concrete

mult :: Gen Mult
mult = fmap Mult concrete

module Penny.Numbers.Concrete.Generators where

import qualified Penny.Numbers.Concrete as C
import Penny.Numbers.Abstract.Unpolar.Generators
import Deka.Dec.Generators
import Test.QuickCheck
import Control.Monad
import Prelude hiding (exponent)

params :: Gen C.Params
params = liftM3 C.Params sign coefficient exponent

concrete :: Gen C.Concrete
concrete = fmap C.concrete params

add :: Gen C.Add
add = fmap C.Add concrete

mult :: Gen C.Mult
mult = fmap C.Mult concrete

arithmeticError :: Gen C.ArithmeticError
arithmeticError = fmap C.ArithmeticError arbitrary

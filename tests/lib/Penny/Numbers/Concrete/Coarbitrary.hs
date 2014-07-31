module Penny.Numbers.Concrete.Coarbitrary where

import qualified Penny.Numbers.Concrete as C
import Deka.Dec.Coarbitrary
import Penny.Numbers.Abstract.Unpolar.Coarbitrary
import Test.QuickCheck
import Prelude hiding (exponent)

params :: C.Params -> Gen b -> Gen b
params (C.Params s c e)
  = sign s . coefficient c . exponent e

concrete :: C.Concrete -> Gen b -> Gen b
concrete c = params . C.params $ c

add :: C.Add -> Gen b -> Gen b
add (C.Add c) = concrete c

mult :: C.Mult -> Gen b -> Gen b
mult (C.Mult c) = concrete c

arithmeticError :: C.ArithmeticError -> Gen b -> Gen b
arithmeticError (C.ArithmeticError s) = coarbitrary s

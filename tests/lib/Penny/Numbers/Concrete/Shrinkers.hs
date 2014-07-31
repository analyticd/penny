module Penny.Numbers.Concrete.Shrinkers where

import qualified Penny.Numbers.Concrete as C
import Deka.Dec.Shrinkers
import Penny.Numbers.Abstract.Unpolar.Shrinkers
import Prelude.Shrinkers
import Prelude hiding (exponent)
import Test.QuickCheck

params :: C.Params -> [C.Params]
params (C.Params s c e) =
  [ C.Params s' c' e' | (s', c', e') <-
    tuple3 sign coefficient exponent (s, c, e) ]

concrete :: C.Concrete -> [C.Concrete]
concrete = map C.concrete . params . C.params

add :: C.Add -> [C.Add]
add = map C.Add . concrete . C.unAdd

mult :: C.Mult -> [C.Mult]
mult = map C.Mult . concrete . C.unMult

arithmeticError :: C.ArithmeticError -> [C.ArithmeticError]
arithmeticError
  = map C.ArithmeticError . shrinkList shrink . C.unArithmeticError

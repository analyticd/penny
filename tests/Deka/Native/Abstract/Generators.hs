module Deka.Native.Abstract.Generators where

import Test.QuickCheck
import Deka.Native.Abstract
import Deka.Dec.Generators
import Control.Monad
import Prelude hiding (exponent)

novem :: Gen Novem
novem = elements [minBound..maxBound]

decem :: Gen Decem
decem = frequency [(1, return D0), (3, fmap Nonem novem)]

decuple :: Gen Decuple
decuple = liftM2 Decuple novem (listOf decem)

aut :: Gen Aut
aut = frequency
  [ (1, return Nil)
  , (3, fmap Plenus decuple)
  ]

firmado :: Gen Firmado
firmado = frequency
  [ (1, return Cero)
  , (3, liftM2 Completo posNeg decuple)
  ]

coefficient :: Gen Coefficient
coefficient = fmap Coefficient aut

exponent :: Gen Exponent
exponent = fmap Exponent firmado

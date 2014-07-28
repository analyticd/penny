module Penny.Numbers.Abstract.Unpolar.Generators where

import Test.QuickCheck
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Natural.Generators
import Deka.Native.Abstract.Generators
import Control.Monad
import Data.Sequence.Generators
import Prelude hiding (seq, exponent)

novDecs :: Gen NovDecs
novDecs = liftM2 NovDecs novem (seq decem)

exponent :: Gen Exponent
exponent = frequency
  [ (1, return ExpZero), (3, fmap ExpNegative novDecs) ]

coefficient :: Gen Coefficient
coefficient = frequency
  [ (1, return CoeZero), (3, fmap CoeNonZero novDecs) ]

zeroesNovDecs :: Gen ZeroesNovDecs
zeroesNovDecs = liftM2 ZeroesNovDecs nonNeg novDecs

decDecs :: Gen DecDecs
decDecs = liftM2 DecDecs decem (seq decem)

hasZeroDigit :: Gen HasZeroDigit
hasZeroDigit = fmap HasZeroDigit arbitrary

zeroDigit :: Gen ZeroDigit
zeroDigit = return ZeroDigit

zeroes :: Gen Zeroes
zeroes = fmap Zeroes pos

uNWhole :: Gen UNWhole
uNWhole = fmap UNWhole novDecs



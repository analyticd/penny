module Penny.Lincoln.Decimal.Abstract.Generators where

import Test.QuickCheck hiding (NonZero(..))
import Control.Monad
import Penny.Lincoln.Decimal.Abstract
import Penny.Lincoln.Decimal.Zero.Generators
import Penny.Lincoln.Decimal.Masuno.Generators
import Penny.Lincoln.Decimal.Frac.Generators

nonZero :: Gen NonZero
nonZero = oneof
  [ fmap NZMasuno masuno
  , fmap NZFrac frac
  ]

figure :: Gen a -> Gen (Figure a)
figure g = liftM2 Figure g nonZero

rep :: Gen a -> Gen (Rep a)
rep g = oneof
  [ fmap RFigure (figure g)
  , fmap RZero zero
  ]

radGroup :: Gen RadGroup
radGroup = elements [ minBound .. maxBound ]

abstract :: Gen a -> Gen (Abstract a)
abstract g = liftM2 Abstract (rep g) radGroup



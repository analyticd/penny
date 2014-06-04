module Penny.Lincoln.Decimal.Abstract.Generators where

import Test.QuickCheck hiding (NonZero(..))
import Control.Monad
import Penny.Lincoln.Decimal.Abstract
import Penny.Lincoln.Decimal.Zero.Generators
import Penny.Lincoln.Decimal.Masuno.Generators
import Penny.Lincoln.Decimal.Frac.Generators
import Penny.Lincoln.Decimal.Side.Generators

nonZero :: Gen NonZero
nonZero = oneof
  [ fmap NZMasuno masuno
  , fmap NZFrac frac
  ]

figure :: Gen Figure
figure = liftM2 Figure side nonZero

rep :: Gen Rep
rep = oneof
  [ fmap RFigure figure
  , fmap RZero zero
  ]

radGroup :: Gen RadGroup
radGroup = elements [ minBound .. maxBound ]

abstract :: Gen Abstract
abstract = liftM2 Abstract rep radGroup



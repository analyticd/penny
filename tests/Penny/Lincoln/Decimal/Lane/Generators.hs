module Penny.Lincoln.Decimal.Lane.Generators where

import Penny.Lincoln.Decimal.Lane
import qualified Penny.Lincoln.Decimal.Side.Generators as SG
import Deka.Native.Abstract.Generators
import Test.QuickCheck
import Control.Monad

lane :: Gen Lane
lane = frequency [(1, return Center),
  (3, fmap NonCenter (liftM2 (,) SG.side decuple))]

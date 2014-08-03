module Penny.Trio.Coarbitrary where

import Penny.Trio
import Penny.Numbers.Abstract.Aggregates.Coarbitrary
import Penny.Numbers.Qty.Coarbitrary
import Barecheck.Util
import Penny.Common.Coarbitrary
import Test.QuickCheck

trio :: Trio -> Gen b -> Gen b
trio tri = case tri of

  QC e c a -> varInt 0 . polarEitherRadix e . commodity c
    . arrangement a

  Q e -> varInt 1 . polarEitherRadix e

  SC s c -> varInt 2 . side s . commodity c

  S s -> varInt 3 . side s

  UC e c a -> varInt 4 . unpolarEitherRadix e . commodity c
    . arrangement a

  U e -> varInt 5 . unpolarEitherRadix e

  C c -> varInt 6 . commodity c

  E -> varInt 7

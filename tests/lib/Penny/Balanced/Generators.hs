{-# LANGUAGE NoImplicitPrelude #-}
module Penny.Balanced.Generators where

import Penny.Balanced
import Test.QuickCheck
import Penny.Numbers.Qty.Generators
import Penny.Common.Generators
import Control.Monad
import Data.Sequence.Generators
import Penny.Numbers.Abstract.Aggregates.Generators

-- | Generates 'Balanced' using 'rBalanced'.
balanced :: Gen m -> Gen (Balanced m)
balanced gm = liftM5 rBalanced side commodity arrangement gm
  (seq (liftM2 (,) polarEitherRadixUnit gm))

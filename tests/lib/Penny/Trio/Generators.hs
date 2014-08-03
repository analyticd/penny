{-# LANGUAGE NoImplicitPrelude #-}
module Penny.Trio.Generators where

import Penny.Trio
import Penny.Common.Generators
import Penny.Numbers.Abstract.Aggregates.Generators
import Test.QuickCheck
import Control.Monad
import Penny.Numbers.Qty.Generators

trio :: Gen Trio
trio = oneof
  [ liftM3 QC polarEitherRadix commodity arrangement
  , fmap Q polarEitherRadix
  , liftM2 SC side commodity
  , fmap S side
  , liftM3 UC unpolarEitherRadix commodity arrangement
  , fmap U unpolarEitherRadix
  , fmap C commodity
  , return E
  ]

-- | Generates a Trio that will create an Ents with one commodity in
-- the inbalances.

oneCommodity :: Gen Trio
oneCommodity = undefined

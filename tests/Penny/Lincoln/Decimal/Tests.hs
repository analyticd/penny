module Penny.Lincoln.Decimal.Tests (testTree) where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import qualified Penny.Lincoln.Decimal.Abstract.Generators as G
import qualified Penny.Lincoln.Decimal.Components.Generators as G
import Penny.Lincoln.Decimal
import Builders

testTree :: TestTree
testTree = testGroup "Penny.Lincoln.Decimal.Tests"
  [ testProperty "abstract -> concrete -> grouped -> concrete"
    invertAbstractGrouped

  , testProperty "abstract -> concrete -> grouped -> concrete"
    invertAbstractUngrouped
  ]

-- | abstract -> concrete -> grouped -> concrete gives the same
-- concrete each time
invertAbstractGrouped :: Property
invertAbstractGrouped = 
  forAll (G.abstract G.side) $ \a ->
  forAll G.radGroup $ \rg ->
  let c = concrete a
  in inversion (grouped rg) concrete c

-- | abstract -> concrete -> ungrouped -> concrete gives the same
-- concrete each time
invertAbstractUngrouped :: Property
invertAbstractUngrouped = 
  forAll (G.abstract G.side) $ \a ->
  forAll G.radGroup $ \rg ->
  let c = concrete a
  in inversion (ungrouped rg) concrete c

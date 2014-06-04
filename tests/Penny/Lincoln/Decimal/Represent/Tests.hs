module Penny.Lincoln.Decimal.Represent.Tests where

import qualified Penny.Lincoln.Decimal.Abstract.Generators as G
import qualified Penny.Lincoln.Decimal.Concrete.Generators as G
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Penny.Lincoln.Decimal.Represent as R
import Penny.Lincoln.Decimal.Concrete
import Builders

testTree :: TestTree
testTree = testGroup "Penny.Lincoln.Decimal.Represent.Tests"
  [ testProperty "concrete -> grouped -> concrete" $
    forAll G.radGroup $ \rg ->
    forAll G.concrete $ \c ->
    inversion (R.grouped rg) concrete c

  , testProperty "concrete -> ungrouped -> concrete" $
    forAll G.radGroup $ \rg ->
    forAll G.concrete $ \c ->
    inversion (R.ungrouped rg) concrete c
  ]

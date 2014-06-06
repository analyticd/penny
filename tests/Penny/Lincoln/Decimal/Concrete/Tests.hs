module Penny.Lincoln.Decimal.Concrete.Tests (testTree) where

import Data.Monoid.Properties
import Test.Tasty
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import qualified Penny.Lincoln.Decimal.Concrete.Generators as G
import Penny.Lincoln.Decimal.Concrete
import Prelude hiding (negate)

testTree :: TestTree
testTree = testGroup "Penny.Lincoln.Decimal.Concrete.Tests"
  [ testGroup "Add" [commutativeMonoidProperties G.add]
  , testGroup "Mult" [commutativeMonoidProperties G.mult]
  , testProperty "x + x - x == x" xPlusXminusX
  , testProperty "negate flips lane" negateFlipsLane
  ]

-- | > x + x - x == x
xPlusXminusX :: Property
xPlusXminusX =
  forAll G.concrete $ \a ->
  ((a `add` a) `subt` a) === a

-- | negate flips the lane
negateFlipsLane :: Property
negateFlipsLane =
  forAll G.concrete $ \a ->
  let lane1 = lane a
      lane2 = lane . negate $ a
  in case lane1 of
      Center -> lane2 === Center
      NonCenter (sd, dc) -> case lane2 of
        Center -> property False
        NonCenter (sd', dc') ->
          sd' === opposite sd .&&. dc === dc'

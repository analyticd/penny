module Copper where

import qualified Copper.Parser as P
import qualified Copper.Render as R
import Test.Tasty (testGroup, TestTree)

testTree :: TestTree
testTree = testGroup "Copper"
  [ P.testTree, R.testTree ]


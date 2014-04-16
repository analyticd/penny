module Main where

import qualified Lincoln as L
import qualified Copper as C
import Test.Tasty (testGroup, TestTree, defaultMain)

testTree :: TestTree
testTree = testGroup "Penny"
  [ L.testTree, C.testTree ]

main :: IO ()
main = defaultMain testTree

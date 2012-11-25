module PennyTest.Penny where

import qualified Test.Framework as TF
import qualified PennyTest.Penny.Lincoln as L
import qualified PennyTest.Penny.Copper as C

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny"
  [ L.tests
  , C.tests
  ]

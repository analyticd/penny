module PennyTest where

import qualified Test.Framework as TF

import qualified PennyTest.Copper as C
import qualified PennyTest.Lincoln as L

tests :: TF.Test
tests = TF.testGroup "PennyTest"
        [C.tests, L.tests]

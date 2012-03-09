module PennyTest where

import qualified Test.Framework as TF

import qualified PennyTest.Copper as C

tests :: TF.Test
tests = TF.testGroup "PennyTest" [C.tests]

module PennyTest where

import Test.Framework (Test)
import Test.Framework.QuickCheck2 (testGroup)

import qualified PennyTests.Lincoln as L

tests :: Test
tests = testGroup "PennyTest" [ L.tests ]

module PennyTest.Penny.Copper where

import qualified PennyTest.Penny.Copper.Parsec as P
import Test.Framework (Test, testGroup)

tests :: Test
tests = testGroup "PennyTest.Penny.Copper"
  [ P.tests
  ]

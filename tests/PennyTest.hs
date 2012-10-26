module PennyTest where

import qualified Test.Framework as TF
import qualified PennyTest.Penny as P

tests :: TF.Test
tests = TF.testGroup "PennyTest"
  [ P.tests ]

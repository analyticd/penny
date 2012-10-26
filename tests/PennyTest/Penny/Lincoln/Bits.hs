module PennyTest.Penny.Lincoln.Bits where

import qualified Test.Framework as TF
import qualified PennyTest.Penny.Lincoln.Bits.Qty as Q

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Lincoln.Bits"
  [ Q.tests ]

module PennyTest.Copper where

import qualified Test.Framework as TF

import qualified PennyTest.Copper.Account as A
import qualified PennyTest.Copper.Qty as Q

tests :: TF.Test
tests = TF.testGroup "Copper" [A.tests, Q.tests]

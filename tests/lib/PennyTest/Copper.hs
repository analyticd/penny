module PennyTest.Copper where

import qualified Test.Framework as TF

import qualified PennyTest.Copper.Account as A

tests :: TF.Test
tests = TF.testGroup "Copper" [A.tests]

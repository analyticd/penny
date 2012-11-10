module PennyTest.Penny.Copper where

import qualified PennyTest.Penny.Copper.Account as Account
import qualified PennyTest.Penny.Copper.Commodity as Commodity

import qualified Test.Framework as TF

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Copper"
  [ Account.tests
  , Commodity.tests
  ]

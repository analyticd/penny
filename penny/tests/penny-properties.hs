{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Test.Tasty
import qualified Penny.Copper.Properties
import qualified Penny.Copper.Date.Properties
import qualified Penny.Lincoln.Properties

main :: IO ()
main = defaultMain $ testGroup "all tests"
  [ Penny.Copper.Properties.testGroup
  , Penny.Copper.Date.Properties.testGroup
  , Penny.Lincoln.Properties.tests
  ]

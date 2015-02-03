{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Test.QuickCheck
import Test.Tasty
import Penny.Copper.Properties

main :: IO ()
main = defaultMain $ testGroup "all tests" [propertiesTestGroup]

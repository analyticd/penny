module Main where

import Penny (tests)
import Test.Framework.Runners.Console (defaultMain)

main :: IO ()
main = defaultMain [tests]

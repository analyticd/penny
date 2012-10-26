module Main where

import PennyTest (tests)
import Test.Framework.Runners.Console (defaultMain)

main :: IO ()
main = defaultMain [tests]

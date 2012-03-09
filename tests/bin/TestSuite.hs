module Main where

import qualified PennyTest as P
import qualified Test.Framework as TF

main :: IO ()
main = TF.defaultMain [P.tests]

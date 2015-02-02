{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import Decrees
import Quickpull
import Test.QuickCheck

args :: Args
args = stdArgs { maxSize = 25 }

main :: IO ()
main = defaultMainWith args decrees



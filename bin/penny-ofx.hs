module Main where

import qualified Penny.Brenner as B
import qualified Paths_penny_bin as PPB

main :: IO ()
main = B.brennerDynamic PPB.version

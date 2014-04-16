module Main where

import Test.Sunlight

ghc v = (v, "ghc-" ++ v, "ghc-pkg-" ++ v)

inputs = TestInputs
  { tiDescription = Nothing
  , tiCabal = "cabal"
  , tiLowest = ghc "7.4.1"
  , tiDefault = map ghc [ "7.4.1", "7.6.3" ]
  , tiTest = [("dist/build/penny-test/penny-test", [])]
  }

main = runTests inputs

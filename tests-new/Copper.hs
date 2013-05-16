module Copper where

import qualified Copper.Parser as P
import qualified Test.QuickCheck as Q

tests :: [(Q.Property -> IO Q.Result) -> IO Bool]
tests = [P.runTests]

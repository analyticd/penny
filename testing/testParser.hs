module Main where

import Penny.Parser
import System.Environment

main :: IO ()
main = do
  (a:[]) <- getArgs
  _testParse a

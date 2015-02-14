-- | Parses the input file given at $1.  Prints the result to standard
-- output.  Any errors are printed to standard error.
module Main where

import Penny.Copper
import System.Environment
import qualified System.IO as IO
import System.Exit
import Text.Show.Pretty

main :: IO ()
main = do
  (fn:[]) <- getArgs
  input <- readFile fn
  case copperParser input of
    Left e -> do
      IO.hPutStr IO.stderr e
      exitFailure
    Right g -> do
      IO.putStrLn . ppShow $ g
      exitSuccess

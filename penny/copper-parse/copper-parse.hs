-- | Parses the input file given at $1.  Prints the result to standard
-- output.  Any errors are printed to standard error.
module Main where

import Penny.Copper
import System.Environment
import qualified System.IO as IO
import System.Exit
import qualified Data.Text.IO as X

main :: IO ()
main = do
  (fn:[]) <- getArgs
  input <- X.readFile fn
  case copperParser input of
    Left e -> do
      IO.hPutStr IO.stderr e
      exitFailure
    Right g -> do
      -- I tried using pretty-show for ppShow here, but it doesn't work
      -- The result is not prettfied at all
      -- IO.putStrLn . ppShow $ g
      IO.putStrLn . show $ g
      exitSuccess

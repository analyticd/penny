-- | Parses the input file given at $1.  Prints the result to standard
-- output.  Any errors are printed to standard error.
module Main where

import Control.Monad (when)
import Penny.Copper.Ast
import System.Environment
import qualified System.IO as IO
import System.Exit

main :: IO ()
main = do
  (fn:[]) <- getArgs
  input <- readFile fn
  let (r, ers, leftover) = parseAst input
  putStrLn . show $ r
  when (not . null $ ers) $ do
    IO.hPutStrLn IO.stderr "Errors:"
    _ <- mapM (IO.hPutStrLn IO.stderr) . map show $ ers
    return ()
  when (not . null $ leftover) $ do
    IO.hPutStrLn IO.stderr "Leftover input errors:"
    _ <- mapM (IO.hPutStrLn IO.stderr) . map show $ leftover
    return ()
  when (not (null leftover) || not (null ers)) exitFailure

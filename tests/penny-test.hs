module Main where

import qualified Lincoln as L
import qualified Copper as C
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified System.Console.MultiArg as MA
import qualified System.Exit as Exit
import qualified Test.QuickCheck as Q

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ "[options]"
  , "runs all QuickCheck tests for Penny."
  , "Returns 0 if all tests succeeded, non-zero otherwise."
  , "Options:"
  , "--size, -s INT"
  , "  Limit QuickCheck size parameter to INT"
  , "--count, -n INT"
  , "  Maximum number of successful tests needed"
  ]

options :: [MA.OptSpec (Q.Args -> Q.Args)]
options =
  [ MA.OptSpec ["size"] "s" . MA.OneArgE $ \s -> do
      i <- MA.reader s
      let f a = a { Q.maxSize = i }
      return f

  , MA.OptSpec ["count"] "n" . MA.OneArgE $ \s -> do
      i <- MA.reader s
      let f a = a { Q.maxSuccess = i }
      return f
  ]

main :: IO ()
main = do
  opts <- MA.simpleHelp help options
          MA.Intersperse
          ( const . Ex.Exception . MA.ErrorMsg
            $ "this command does not accept positional arguments")
  let args = foldl (flip (.)) id opts Q.stdArgs
      runner = Q.quickCheckWithResult args
      acts = map ($ runner) allTests
  bools <- sequence acts
  if and bools
    then Exit.exitSuccess
    else Exit.exitFailure

allTests :: [(Q.Property -> IO Q.Result) -> IO Bool]
allTests = [ L.runTests ] ++ C.tests

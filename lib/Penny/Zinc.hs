-- | Zinc - the Penny command-line interface
module Penny.Zinc (runPenny, P.T(..), P.ColorToFile(..),
                   P.defaultFromRuntime) where

import System.Console.MultiArg.GetArgs (getArgs)

import qualified Penny.Cabin.Interface as I
import qualified Penny.Shield as S
import qualified Penny.Zinc.Parser as P

runPenny ::
  S.Runtime
  -> (S.Runtime -> P.T)
  -> [I.Report]
  -> IO ()
runPenny rt df rs = do
  as <- getArgs
  let d = df rt
  P.parseAndPrint d rs rt as

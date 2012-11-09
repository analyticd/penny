-- | Zinc - the Penny command-line interface
module Penny.Zinc (runPenny, P.T(..),
                   P.defaultFromRuntime) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified System.Console.MultiArg.Prim as M
import System.Console.MultiArg.GetArgs (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)

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
  case M.parse as (P.parser rt df rs) of
    Ex.Exception e -> do
      hPutStrLn stderr . show $ e
      exitFailure
    Ex.Success g -> g

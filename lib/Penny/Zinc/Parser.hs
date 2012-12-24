module Penny.Zinc.Parser
  ( Defaults.T(..)
  , Defaults.defaultFromRuntime
  , parseAndPrint
  ) where


import Control.Applicative ((<*>),  pure)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Monoid (mappend, mconcat)
import qualified Data.Text as X
import qualified Data.Text.IO as StrictIO
import qualified System.Console.MultiArg as MA
import System.Exit (exitSuccess, exitFailure)
import qualified System.IO as IO
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Shield as S
import qualified Penny.Zinc.Help as Help
import qualified Penny.Zinc.Parser.Filter as F
import qualified Penny.Zinc.Parser.Ledgers as L
import qualified Penny.Zinc.Parser.Defaults as Defaults

parseCommandLine
  :: Defaults.T
  -> [I.Report]
  -> S.Runtime
  -> [String]
  -> Ex.Exceptional MA.Error
     (F.GlobalResult, Either [()] ((), (Defaults.ColorToFile, I.ParseResult)))
parseCommandLine df rs rt ss =
  MA.modes (F.allOpts (S.currentTime rt)) (F.processGlobal df)
           (whatMode rt rs) ss

whatMode
  :: S.Runtime
  -> [I.Report]
  -> F.GlobalResult
  -> Either (a -> ()) [MA.Mode () (Defaults.ColorToFile, I.ParseResult)]
whatMode rt rs gr =
  case gr of
    F.NeedsHelp -> Left $ const ()
    F.RunPenny (F.FilterOpts fty cs sf ctf) ->
      let prs = map snd rs
                <*> pure rt
                <*> pure cs
                <*> pure fty
                <*> pure sf
          toCtf r = (ctf, r)
      in Right $ map (fmap toCtf) prs

handleParseResult
  :: S.Runtime
  -> [I.Report]
  -> Ex.Exceptional MA.Error
     (a, Either b (c, (Defaults.ColorToFile, I.ParseResult)))
  -> IO ()
handleParseResult rt rs r =
  let showErr e = do
        IO.hPutStr IO.stderr $ "penny: error: " ++ e
        exitFailure
  in case r of
    Ex.Exception e -> do
      IO.hPutStr IO.stderr $ MA.formatError "penny" e
      exitFailure
    Ex.Success (_, ei) ->
      case ei of
        Left _ -> do
          StrictIO.putStr (helpText rs)
          exitSuccess
        Right (_, (ctf, ex)) -> case ex of
          Ex.Exception s -> showErr s
          Ex.Success (fns, pr) -> do
            ledgers <- L.readLedgers fns
            (txns, pps) <- Ex.switch showErr return
                           $ L.parseLedgers ledgers
            let term = if Defaults.unColorToFile ctf
                       then Chk.TermFromEnv
                       else Chk.autoTerm rt
            Ex.switch (showErr . X.unpack)
              (Chk.printChunks term) $ pr txns pps

helpText ::
  [I.Report]
  -> X.Text
helpText = mappend Help.help . mconcat . fmap fst


parseAndPrint
  :: Defaults.T
  -> [I.Report]
  -> S.Runtime
  -> [String]
  -> IO ()
parseAndPrint df rs rt ss =
  handleParseResult rt rs
  $ parseCommandLine df rs rt ss

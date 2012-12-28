module Penny.Zinc.Parser
  ( Defaults.T(..)
  , Defaults.ColorToFile(..)
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
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Shield as S
import qualified Penny.Zinc.Help as Help
import qualified Penny.Zinc.Parser.Filter as F
import qualified Penny.Zinc.Parser.Ledgers as L
import qualified Penny.Zinc.Parser.Defaults as Defaults

data DisplayOpts = DisplayOpts
  { colorToFile :: Defaults.ColorToFile
  , scheme :: E.Scheme
  }

toDisplayOpts :: F.FilterOpts -> DisplayOpts
toDisplayOpts o = DisplayOpts (F.colorToFile o) (F.scheme o)

parseCommandLine
  :: Defaults.T
  -> [I.Report]
  -> S.Runtime
  -> [String]
  -> Ex.Exceptional MA.Error
     (F.GlobalResult, Either [()] (DisplayOpts, I.ParseResult))
parseCommandLine df rs rt ss =
  MA.modes (F.allOpts (S.currentTime rt)) (F.processGlobal df)
           (whatMode rt rs) ss

whatMode
  :: S.Runtime
  -> [I.Report]
  -> F.GlobalResult
  -> Either (a -> ()) [MA.Mode (DisplayOpts, I.ParseResult)]
whatMode rt rs gr =
  case gr of
    F.NeedsHelp -> Left $ const ()
    F.RunPenny fo@(F.FilterOpts fty cs sf _ _) ->
      let prs = map snd rs
                <*> pure rt
                <*> pure cs
                <*> pure fty
                <*> pure sf
      in Right $ map (fmap (\r -> ((toDisplayOpts fo), r))) prs

handleParseResult
  :: S.Runtime
  -> [I.Report]
  -> Ex.Exceptional MA.Error
     (a, Either b (DisplayOpts, I.ParseResult))
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
        Right (dispOpts, ex) -> case ex of
          Ex.Exception s -> showErr s
          Ex.Success (fns, pr) -> do
            ledgers <- L.readLedgers fns
            (txns, pps) <- Ex.switch showErr return
                           $ L.parseLedgers ledgers
            let term = if Defaults.unColorToFile . colorToFile $ dispOpts
                       then Chk.TermFromEnv
                       else Chk.autoTerm rt
                sch = scheme dispOpts
            Ex.switch (showErr . X.unpack)
              (printChunks term sch) $ pr txns pps

printChunks
  :: Chk.Term
  -> E.Scheme
  -> [E.PreChunk]
  -> IO ()
printChunks t s =
  Chk.printChunks t
  . map (E.makeChunk s)

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

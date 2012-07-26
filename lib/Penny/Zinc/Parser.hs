module Penny.Zinc.Parser (
  Defaults.T(..)
  , Defaults.defaultFromRuntime
  , parser
  ) where

import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Monoid (mappend, mconcat)
import qualified Data.Text as X
import qualified Data.Text.IO as StrictIO
import qualified Data.Text.Lazy.IO as LazyIO
import System.Console.MultiArg.Prim (Parser)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr, hPutStrLn)
import qualified Penny.Cabin.Interface as I
import qualified Penny.Shield as S
import qualified Penny.Zinc.Help as Help
import qualified Penny.Zinc.Parser.Filter as F
import qualified Penny.Zinc.Parser.Report as R
import qualified Penny.Zinc.Parser.Ledgers as L
import qualified Penny.Zinc.Parser.Defaults as Defaults

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (RadGroup)

-- | Parses all command line options and arguments. Returns an IO
-- action which will print appropriate error messages if something
-- failed, or will print a report and exit successfully if everything
-- went well.
parser ::
  S.Runtime
  -> DefaultTimeZone
  -> RadGroup
  -> (S.Runtime -> Defaults.T)
  -> [I.Report]
  -> Parser (IO ())
parser rt dtz rg getDf rs = do
  let df = getDf rt
  errFilt <- F.parseFilter df
  case errFilt of
    Ex.Exception e -> return $ do
      hPutStrLn stderr $ ("penny: error: " ++ show e)
      exitFailure
    Ex.Success r -> case r of
      Left F.NeedsHelp -> return $ do
        StrictIO.putStrLn . helpText $ rs
        exitSuccess
      Right rslt -> parseReportsAndFilesAndPrint rt dtz rg rs rslt
        

-- | Returns an IO action that will parse the report options and the
-- files on the command line and print the resulting report.
parseReportsAndFilesAndPrint ::
  S.Runtime
  -> DefaultTimeZone
  -> RadGroup
  -> [I.Report]
  -> F.Result
  -> Parser (IO ())
parseReportsAndFilesAndPrint rt dtz rg rs rslt = do
  let (F.Result factory sensitive sortFilt) = rslt
  parserFunc <- R.report rt rs
  let reportFunc = parserFunc sensitive factory
  filenames <- L.filenames
  return $ do
    ledgers <- L.readLedgers filenames
    let parsed = L.parseLedgers dtz rg ledgers
    case parsed of
      Ex.Exception e -> do
        hPutStrLn stderr . show $ e
        exitFailure
      Ex.Success (txns, prices) -> do
        let boxes = sortFilt txns
        case reportFunc boxes prices of
          Ex.Exception bad -> do
            StrictIO.putStrLn bad
            exitFailure
          Ex.Success good -> do
            LazyIO.putStr good
            exitSuccess

helpText ::
  [I.Report]
  -> X.Text
helpText = mappend Help.help . mconcat . fmap I.help


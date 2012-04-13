-- | Zinc - the Penny command-line interface
module Penny.Zinc where

import Control.Applicative ((<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Monoid (mappend, mconcat)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as XL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Traversable as T
import System.Console.MultiArg.Prim (parseE)
import System.Console.MultiArg.GetArgs (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

import qualified Penny.Cabin.Interface as I
import qualified Penny.Copper as Cop
import Penny.Lincoln.Boxes (PostingBox, postingBoxes)
import qualified Penny.Liberty.Error as LE
import qualified Penny.Liberty.Types as T
import qualified Penny.Shield as S
import qualified Penny.Zinc.Error as ZE
import qualified Penny.Zinc.Help as H
import qualified Penny.Zinc.Parser as P
import qualified Penny.Zinc.Parser.Ledgers as L

reportChunk ::
  Cop.DefaultTimeZone
  -> Cop.RadGroup
  -> NE.NonEmpty (L.Filename, Text)
  -> ([PostingBox] -> [T.PostingInfo])
  -> I.ReportFunc
  -> Ex.Exceptional ZE.Error XL.Text
reportChunk dtz rg fs filt rf = do
  ps <- L.combineData <$> T.traverse (L.parseLedger dtz rg) fs
  let processedPostings = filt . postingBoxes . fst $ ps
  case rf processedPostings (snd ps) of
    Ex.Exception e -> Ex.Exception (ZE.ReportError e)
    Ex.Success g -> Ex.Success g

zincMain ::
  Cop.DefaultTimeZone
  -> Cop.RadGroup
  -> NE.NonEmpty I.Report
  -> IO ()
zincMain dtz rg rs = do
  rt <- S.runtime
  as <- fmap (fmap pack) getArgs
  multiArgRes <- case parseE as (P.parser rt dtz rg rs) of
    Ex.Exception e -> parseErrorExit e
    Ex.Success g -> return g
  parseRes <- case multiArgRes of
    Left _ -> 
      TIO.putStrLn (helpText rs)
      >> exitSuccess
    Right r -> return r
  legs <- L.readLedgers (P.filenames parseRes)
  let filt = P.sorterFilterWithPost parseRes
      rpt = P.reportFunc parseRes
  case reportChunk dtz rg legs filt rpt of
    Ex.Exception err -> do
      TIO.putStrLn . ZE.printError $ err
      exitFailure
    Ex.Success g -> LIO.putStr g >> exitSuccess


helpText ::
  (F.Foldable f, Functor f)
  => f I.Report
  -> Text
helpText = mappend H.help . mconcat . F.toList . fmap I.help

parseErrorExit :: LE.Error -> IO a
parseErrorExit e = TIO.hPutStrLn stderr (LE.display e) >> exitFailure

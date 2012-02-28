-- | Zinc - the Penny command-line interface
module Penny.Zinc where

import Control.Applicative ((<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as T
import System.Console.MultiArg.Prim (parseE)
import System.Console.MultiArg.GetArgs (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Interface as I
import qualified Penny.Copper as Cop
import Penny.Lincoln.Boxes (PostingBox, PriceBox, postingBoxes)
import qualified Penny.Liberty.Error as LE
import qualified Penny.Liberty.Types as T
import qualified Penny.Shield as S
import qualified Penny.Zinc.Error as ZE
import qualified Penny.Zinc.Parser as P
import qualified Penny.Zinc.Parser.Ledgers as L

reportChunk ::
  Cop.DefaultTimeZone
  -> Cop.Radix
  -> Cop.Separator
  -> NE.NonEmpty (L.Filename, Text)
  -> ([PostingBox] -> [T.PostingInfo])
  -> I.ReportFunc
  -> Ex.Exceptional ZE.Error Col.Chunk
reportChunk dtz rad sep fs filt rf = do
  ps <- L.combineData <$> T.traverse (L.parseLedger dtz rad sep) fs
  let processedPostings = filt . postingBoxes . fst $ ps
  case rf processedPostings (snd ps) of
    Ex.Exception e -> Ex.Exception (ZE.ReportError e)
    Ex.Success g -> Ex.Success g

zincMain ::
  Cop.DefaultTimeZone
  -> Cop.Radix
  -> Cop.Separator
  -> NE.NonEmpty I.Report
  -> IO ()
zincMain dtz rad sep rs = do
  rt <- S.runtime
  as <- fmap (fmap pack) getArgs
  case parseE as (P.parser rt dtz rad sep rs) of
    Ex.Exception e -> parseErrorExit e
    Ex.Success g -> return ()

parseErrorExit :: LE.Error -> IO ()
parseErrorExit e = TIO.hPutStrLn stderr (LE.display e) >> exitFailure

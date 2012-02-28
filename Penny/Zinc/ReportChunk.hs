module Penny.Zinc.ReportChunk where

import Control.Applicative ((<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Traversable as T

import qualified Penny.Cabin.Colors as Col
import qualified Penny.Cabin.Interface as I
import qualified Penny.Copper as Cop
import Penny.Lincoln.Boxes (PostingBox, PriceBox, postingBoxes)
import qualified Penny.Liberty.Types as T
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


module Penny.Zinc.Parser where

import qualified Data.List.NonEmpty as NE
import System.Console.MultiArg.Prim (ParserE)
import qualified Penny.Liberty.Types as T
import Penny.Cabin.Colors (ColorPref)
import qualified Penny.Cabin.Interface as I
import qualified Penny.Liberty.Error as E
import qualified Penny.Shield as S
import qualified Penny.Zinc.Parser.Filter as F
import qualified Penny.Zinc.Parser.Report as R
import qualified Penny.Zinc.Parser.Ledgers as L

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (RadGroup)
import Penny.Lincoln.Boxes (PostingBox)

data Result =
  Result { sorterFilterWithPost :: [PostingBox] -> [T.PostingInfo]
         , reportFunc :: I.ReportFunc
         , colors :: ColorPref
         , filenames :: NE.NonEmpty L.Filename }

parser ::
  S.Runtime
  -> DefaultTimeZone
  -> RadGroup
  -> NE.NonEmpty I.Report
  -> ParserE E.Error (Either F.NeedsHelp Result)
parser rt dtz rg rpts = do
  fr <- F.parseFilter dtz (S.currentTime rt) rg
  case fr of
    Left h -> return $ Left h
    Right filtResult -> do
      let sensitive = F.resultSensitive filtResult
          factory = F.resultFactory filtResult
          filt = F.sorterFilterer filtResult
      (rpt, cs) <- R.report rt rpts sensitive factory
      fns <- L.filenames
      return . Right $ Result filt rpt cs fns
  

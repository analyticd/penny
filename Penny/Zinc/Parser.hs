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
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)

data Result =
  Result { filter :: [PostingBox] -> [T.PostingInfo]
         , reportFunc :: I.ReportFunc
         , colors :: ColorPref
         , filenames :: NE.NonEmpty L.Filename }

parser ::
  S.Runtime
  -> DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> NE.NonEmpty I.Report
  -> ParserE E.Error Result
parser rt dtz dt rad sep rpts = do
  filtResult <- F.parseFilter dtz dt rad sep
  let sensitive = F.resultSensitive filtResult
      factory = F.resultFactory filtResult
      filt = F.resultFilter filtResult
  (rpt, cs) <- R.report rt rpts sensitive factory
  fns <- L.filenames
  return $ Result filt rpt cs fns
  

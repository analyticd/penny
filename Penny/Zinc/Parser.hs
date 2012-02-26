module Penny.Zinc.Parser where

import qualified Data.List.NonEmpty as NE
import System.Console.MultiArg.Prim (ParserE)
import qualified Penny.Liberty.Types as T
import qualified Penny.Cabin.Class as C
import Penny.Cabin.Colors (Colors)
import qualified Penny.Liberty.Error as E
import qualified Penny.Zinc.Filter as F
import qualified Penny.Zinc.Report as R
import qualified Penny.Zinc.Ledgers as L

import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PostingBox)

data Result =
  Result { filter :: [PostingBox] -> [T.PostingInfo]
         , reportFunc :: C.ReportFunc
         , colors :: Colors
         , filenames :: NE.NonEmpty L.Filename }

parser ::
  DefaultTimeZone
  -> DateTime
  -> Radix
  -> Separator
  -> NE.NonEmpty C.Report
  -> ParserE E.Error Result
parser dtz dt rad sep rpts = do
  filtResult <- F.parseFilter dtz dt rad sep
  let sensitive = F.resultSensitive filtResult
      factory = F.resultFactory filtResult
      filt = F.resultFilter filtResult
  (rpt, cs) <- R.report rpts sensitive factory
  fns <- L.filenames
  return $ Result filt rpt cs fns
  

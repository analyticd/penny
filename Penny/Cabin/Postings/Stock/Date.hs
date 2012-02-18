module Penny.Cabin.Postings.Stock.Date where

import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Data.Time as T

import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Cabin.Postings.Stock.Util as U
import Penny.Cabin.Postings.Stock.Columns (Fields, date)
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q

showDateOnly :: Bits.DateTime -> X.Text
showDateOnly dt = X.intercalate (X.singleton '-') [y, m, d] where
  (yi, mi, di) = T.toGregorian . T.utctDay . Bits.unDateTime $ dt
  y = X.pack . show $ yi
  m = X.justifyRight 2 '0' (X.pack . show $ mi)
  d = X.justifyRight 2 '0' (X.pack . show $ di)

mainFormula ::
  (Bits.DateTime -> X.Text)
  -> Fields Bool
  -> C.BaseColors
  -> B.Formula Columns.C
mainFormula render fields c = B.FGrowToFit f where
  f = if date fields
      then \_ _ p ci -> growFormula render c p ci
      else U.zeroGrowToFit

spacerFormula :: Fields Bool -> C.BaseColors -> B.Formula Columns.C
spacerFormula fields c = B.FGrowToFit f where
  f = if date fields
      then \_ _ p ci -> spacer c p ci
      else U.zeroGrowToFit

growFormula ::
  (Bits.DateTime -> X.Text)
  -> C.BaseColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
growFormula render colors p ci = let
  ntct = U.Justified
  sq = Seq.singleton . render . Q.dateTime . B.postingBox $ p
  just = R.LeftJustify
  in U.makeGrowingCell ntct colors p ci just sq

spacer ::
  C.BaseColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
spacer colors p ci = U.makeSpacerCell U.Justified colors p ci True

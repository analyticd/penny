module Penny.Cabin.Postings.Stock.PostingCommodity where

import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Data.Time as T

import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Cabin.Postings.Stock.Util as U
import Penny.Cabin.Postings.Stock.Columns (Fields, postingCmdty)
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Predicates as P

mainFormula :: Fields Bool
               -> C.DrCrColors
               -> B.Formula Columns.C
mainFormula fields c = B.FGrowToFit f where
  f = if postingCmdty fields
      then (\_ _ p ci -> growFormula c p ci)
      else U.zeroGrowToFit

spacerFormula :: Fields Bool
                 -> C.DrCrColors
                 -> B.Formula Columns.C
spacerFormula fields c = B.FGrowToFit f where
  f = if postingCmdty fields
      then \_ _ p ci -> spacer c p ci
      else U.zeroGrowToFit

spacer ::
  C.DrCrColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
spacer colors p ci = U.makeSpacerCell U.Overran dcColors p ci True where
  dc = Q.drCr . B.postingBox $ p
  dcColors = C.drCrToBaseColors dc colors

growFormula ::
  C.DrCrColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
growFormula colors p ci = let
  ntct = U.Overran
  cmdty = Q.commodity . B.postingBox $ p
  sq = Seq.singleton s where
    s = P.text . P.Delimited (X.pack ":") . P.textList $ cmdty
  dc = Q.drCr . B.postingBox $ p
  bc = C.drCrToBaseColors dc colors
  just = R.RightJustify
  in U.makeGrowingCell ntct bc p ci just sq

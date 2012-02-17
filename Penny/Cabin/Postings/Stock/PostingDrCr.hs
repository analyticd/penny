module Penny.Cabin.Postings.Stock.PostingDrCr where

import Data.Sequence (singleton)
import Data.Text (pack)

import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Cabin.Postings.Stock.Util as U
import Penny.Cabin.Postings.Stock.Columns (Fields, postingDrCr)
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q

mainFormula :: Fields Bool
               -> C.DrCrColors
               -> B.Formula Columns.C
mainFormula fields c = B.FGrowToFit f where 
  f = if postingDrCr fields
      then (\_ _ p ci -> growFormula c p ci)
      else U.zeroGrowToFit

spacerFormula :: Fields Bool -> C.DrCrColors -> B.Formula Columns.C
spacerFormula fields c = B.FGrowToFit f where
  f = if postingDrCr fields
      then \_ _ p ci -> spacer c p ci
      else U.zeroGrowToFit

spacer:: 
  C.DrCrColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
spacer colors p ci = let
  ntct = U.Overran
  dc = Q.drCr . B.postingBox $ p
  bc = C.drCrToBaseColors dc colors
  just = R.LeftJustify
  s = singleton (pack " ")
  in U.makeGrowingCell ntct bc p ci just s

growFormula ::
  C.DrCrColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
growFormula colors p ci = let
  ntct = U.Overran
  pstg = B.postingBox p
  dc = Q.drCr pstg
  bc = C.drCrToBaseColors dc colors
  just = R.LeftJustify
  sq = singleton . pack $ s where
    s = case dc of
      Bits.Debit -> "Dr"
      Bits.Credit -> "Cr"
  in U.makeGrowingCell ntct bc p ci just sq

module Penny.Cabin.Postings.Stock.PostingDrCr where

import Data.Sequence (singleton, empty)
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
  -> B.CellInfo c
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried c) -> R.Cell)
spacer colors p ci = pair where
  pair = if U.isTopRow p ci
         then full
         else (B.ColumnWidth 0, const R.zeroCell)
  full = (cw, f) where
    cw = B.ColumnWidth 1
    color = pickColor p colors
    f _ = R.Cell R.LeftJustify (C.Width 1) color empty
  
growFormula ::
  C.DrCrColors
  -> B.PostingInfo
  -> B.CellInfo c
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried c) -> R.Cell)
growFormula colors p ci = pair where
  pair =
    if U.isTopRow p ci
    then full
    else (B.ColumnWidth 0, const R.zeroCell)
  full = (cw, f) where
    cw = B.ColumnWidth 2
    pstg = B.postingBox p
    dc = Q.drCr pstg
    color = pickColor p colors
    f _ = R.Cell R.LeftJustify (C.Width 2) color cs
    cs = singleton (C.chunk color (pack dcTxt))
    dcTxt = case dc of
      Bits.Debit -> "Dr"
      Bits.Credit -> "Cr"

pickColor :: B.PostingInfo -> C.DrCrColors -> C.TextSpec
pickColor p = colorF where
  colorF = case (odd . B.unPostingNum . B.postingNum $ p, dc) of
    (True, Bits.Debit) -> C.oddDebit
    (True, Bits.Credit) -> C.oddCredit
    (False, Bits.Debit) -> C.evenDebit
    (False, Bits.Credit) -> C.evenCredit
  dc = Q.drCr . B.postingBox $ p

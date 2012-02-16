module Penny.Cabin.Postings.Stock.PostingDrCr where

import Data.Sequence (singleton, empty)
import Data.Text (pack)

import Penny.Lincoln.Boxes (PostingBox, PriceBox)

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q

data DrCrColors =
  DrCrColors { evenDebit :: C.TextSpec
             , evenCredit :: C.TextSpec
             , oddDebit :: C.TextSpec
             , oddCredit :: C.TextSpec }

mainFormula :: DrCrColors -> B.Formula Columns.C
mainFormula = B.FGrowToFit . growFormula

spacerFormula :: DrCrColors -> B.Formula Columns.C
spacerFormula = B.FGrowToFit . spacer

spacer:: 
  DrCrColors
  -> B.ReportWidth
  -> [PriceBox]
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried c) -> R.Cell)
spacer colors _ _ p ci = (cw, f) where
  cw = B.ColumnWidth 1
  color = pickColor p colors
  f _ = R.Cell R.LeftJustify (C.Width 1) color empty
  
growFormula ::
  DrCrColors
  -> B.ReportWidth
  -> [PriceBox]
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried c) -> R.Cell)
growFormula colors _ _ p ci = (cw, f) where
  cw = B.ColumnWidth 2
  pstg = B.postingBox p
  dc = Q.drCr pstg
  color = pickColor p colors
  f _ = R.Cell R.LeftJustify (C.Width 2) color cs
  cs = singleton (C.chunk color (pack dcTxt))
  dcTxt = undefined

pickColor :: B.PostingInfo -> DrCrColors -> C.TextSpec
pickColor p = colorF where
  colorF = case (odd . B.unPostingNum . B.postingNum $ p, dc) of
    (True, Bits.Debit) -> oddDebit
    (True, Bits.Credit) -> oddCredit
    (False, Bits.Debit) -> evenDebit
    (False, Bits.Credit) -> evenCredit
  dc = Q.drCr . B.postingBox $ p

module Penny.Cabin.Postings.Stock.LineNum where

import Data.Sequence (singleton)
import Data.Text (pack)
import qualified Data.Text as X

import qualified Penny.Cabin.Postings.Base.Base as B
import Penny.Cabin.Postings.Base.Combinator (lJustCell)
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.Queries as Q

mainFormula :: C.BaseColors -> B.Formula Columns.C
mainFormula bc = B.FGrowToFit (\_ _ p ci -> growFormula bc p ci)

spacerFormula :: C.BaseColors -> B.Formula Columns.C
spacerFormula bc = B.FGrowToFit (\_ _ p ci -> spacer bc p ci)

growFormula ::
  C.BaseColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
growFormula colors p ci = (cw, f) where
  t = case Q.postingLine . B.postingBox $ p of
    Nothing -> X.empty
    (Just pl) -> pack . show . M.unLine . M.unPostingLine $ pl
  cw = B.ColumnWidth . X.length $ t
  col = B.cellCol ci
  ts = C.colors p colors
  ks = singleton (C.chunk ts t)
  f = lJustCell col ts ks

spacer ::
  C.BaseColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
spacer colors p ci = (cw, f) where
  t = case Q.postingLine . B.postingBox $ p of
    Nothing -> X.empty
    (Just _) -> X.singleton ' '
  cw = B.ColumnWidth . X.length $ t
  col = B.cellCol ci
  ts = C.colors p colors
  ks = singleton (C.chunk ts t)
  f = lJustCell col ts ks

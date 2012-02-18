module Penny.Cabin.Postings.Stock.Number where

import qualified Data.Sequence as Seq
import qualified Data.Text as X
import qualified Data.Time as T

import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Cabin.Postings.Stock.Util as U
import Penny.Cabin.Postings.Stock.Columns (Fields, number)
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q

mainFormula :: Fields Bool
               -> C.BaseColors
               -> B.Formula Columns.C
mainFormula fields c = B.FGrowToFit f where
  f = if number fields
      then (\_ _ p ci -> growFormula c p ci)
      else U.zeroGrowToFit

growFormula ::
  C.BaseColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
growFormula = undefined

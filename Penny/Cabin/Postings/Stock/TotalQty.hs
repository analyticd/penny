module Penny.Cabin.Postings.Stock.TotalQty where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as X

import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Cabin.Postings.Stock.Util as U
import Penny.Cabin.Postings.Stock.Columns (Fields, totalQty)
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Queries as Q

mainFormula ::
  (B.PostingInfo -> X.Text)
  -> Fields Bool
  -> C.DrCrColors
  -> B.Formula Columns.C
mainFormula render fields c = B.FGrowToFit f where 
  f = if totalQty fields
      then (\_ _ p ci -> growFormula render c p ci)
      else U.zeroGrowToFit

growFormula ::
  (B.PostingInfo -> X.Text)
  -> C.DrCrColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
growFormula render colors p ci = let
  ntct = U.Justified
  ngts = Seq.fromList . Map.elems . Bal.unBalance . B.balance $ p
  chunks = fmap (qtyLine render colors p) ngts
  j = R.RightJustify
  dc = Q.drCr . B.postingBox $ p
  bc = C.drCrToBaseColors dc colors
  in U.makeColorfulGrowingCell ntct bc p ci j chunks

qtyLine ::
  (B.PostingInfo -> X.Text)
  -> C.DrCrColors
  -> B.PostingInfo
  -> Bal.Nought
  -> CC.Chunk
qtyLine render colors p ngt = CC.chunk ts t where
  ts = C.colors p . C.noughtToBaseColors colors $ ngt
  t = render p


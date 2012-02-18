module Penny.Cabin.Postings.Stock.TotalDrCr where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as X

import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Cabin.Postings.Stock.Util as U
import Penny.Cabin.Postings.Stock.Columns (Fields, totalDrCr)
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q

mainFormula :: Fields Bool
               -> C.DrCrColors
               -> B.Formula Columns.C
mainFormula fields c = B.FGrowToFit f where 
  f = if totalDrCr fields
      then (\_ _ p ci -> growFormula c p ci)
      else U.zeroGrowToFit

spacerFormula :: Fields Bool -> C.BaseColors -> B.Formula Columns.C
spacerFormula fields c = B.FGrowToFit f where
  f = if totalDrCr fields
      then \_ _ p ci -> spacer c p ci
      else U.zeroGrowToFit

spacer:: 
  C.BaseColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
spacer colors p ci = U.makeSpacerCell U.Justified colors p ci True

growFormula ::
  C.DrCrColors
  -> B.PostingInfo
  -> B.CellInfo Columns.C
  -> (B.ColumnWidth,
      B.Table Columns.C (B.PostingInfo, B.Queried Columns.C) -> R.Cell)
growFormula colors p ci = let
  ntct = U.Justified
  noughts = Seq.fromList . Map.elems . Bal.unBalance . B.balance $ p
  chunks = fmap (drCrLine colors p) noughts
  j = R.LeftJustify
  dc = Q.drCr . B.postingBox $ p
  bc = C.drCrToBaseColors dc colors
  in U.makeColorfulGrowingCell ntct bc p ci j chunks

drCrLine :: C.DrCrColors -> B.PostingInfo -> Bal.Nought -> CC.Chunk
drCrLine colors p n = CC.chunk ts t where
  ts = C.colors p . C.noughtToBaseColors colors $ n
  t = X.pack $ case n of
    Bal.Zero -> "--"
    Bal.NonZero column -> case Bal.drCr column of
      Bits.Debit -> "Dr"
      Bits.Credit -> "Cr"

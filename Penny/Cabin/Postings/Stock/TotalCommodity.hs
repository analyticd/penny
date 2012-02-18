module Penny.Cabin.Postings.Stock.TotalCommodity where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as X

import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Postings.Stock.Columns as Columns
import qualified Penny.Cabin.Postings.Stock.Util as U
import Penny.Cabin.Postings.Stock.Columns (Fields, totalCmdty)
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Predicates as P

mainFormula :: Fields Bool
               -> C.DrCrColors
               -> B.Formula Columns.C
mainFormula fields c = B.FGrowToFit f where 
  f = if totalCmdty fields
      then (\_ _ p ci -> growFormula c p ci)
      else U.zeroGrowToFit

spacerFormula :: Fields Bool -> C.BaseColors -> B.Formula Columns.C
spacerFormula fields c = B.FGrowToFit f where
  f = if totalCmdty fields
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
  ctyNgts = Seq.fromList . Map.assocs . Bal.unBalance . B.balance $ p
  chunks = fmap (cmdtyLine colors p) ctyNgts
  j = R.RightJustify
  dc = Q.drCr . B.postingBox $ p
  bc = C.drCrToBaseColors dc colors
  in U.makeColorfulGrowingCell ntct bc p ci j chunks

cmdtyLine ::
  C.DrCrColors
  -> B.PostingInfo
  -> (Bits.Commodity, Bal.Nought)
  -> CC.Chunk
cmdtyLine colors p (cty, ngt) = CC.chunk ts t where
  ts = C.colors p . C.noughtToBaseColors colors $ ngt
  t = P.text . P.Delimited (X.singleton ':') . P.textList $ cty


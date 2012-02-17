module Penny.Cabin.Postings.Stock.Util where

import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Row as R

zeroGrowToFit :: a -> b -> c -> d -> (B.ColumnWidth, e -> R.Cell)
zeroGrowToFit _ _ _ _ = (B.ColumnWidth 0, const (R.zeroCell))

rowsPerRecord :: Int
rowsPerRecord = 4

isOffset :: Int -> B.PostingInfo -> B.CellInfo c -> Bool
isOffset offset p c = rowNum - rowsPerRecord * visNum == offset where
  rowNum = B.unRowNum . B.cellRow $ c
  visNum = B.unVisibleNum . B.visibleNum $ p



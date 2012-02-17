module Penny.Cabin.Postings.Stock.Util where

import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Stock.Colors as C
import qualified Penny.Cabin.Postings.Base.Row as R

zeroGrowToFit :: a -> b -> c -> d -> (B.ColumnWidth, e -> R.Cell)
zeroGrowToFit _ _ _ _ = (B.ColumnWidth 0, const (R.zeroCell))

module Penny.Cabin.Postings.Combinator where

import Data.Array (Array, Ix)
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Map as Map
import qualified Data.Map as M
import qualified Data.Table as Ta
import Data.Text (Text)
import qualified Data.Text as X

import Penny.Cabin.Postings.Combinator.Allocate (allocate)
import qualified Penny.Cabin.Postings.Base as B
import qualified Penny.Cabin.Postings.Row as R
import qualified Penny.Cabin.Colors as C

-- | Calculate the widest cell in a column.
widest ::
  Ix c
  => B.Table c (B.PostingInfo, B.Queried c)
  -> c
  -> B.ColumnWidth
widest t c = F.foldr f (B.ColumnWidth 0) col where
  col = Ta.OneDim $ Ta.column t c
  f (_, q) widestSoFar = case q of
    B.QGrowToFit (cw, _) -> max cw widestSoFar
    _ -> widestSoFar

lJustCell ::
  Ix c
  => c
  -> C.TextSpec
  ->  B.Table c (B.PostingInfo, B.Queried c)
  -> R.Cell
lJustCell = cell R.LeftJustify
  
rJustCell ::
  Ix c
  => c
  -> C.TextSpec
  -> B.Table c (B.PostingInfo, B.Queried c)
  -> R.Cell
rJustCell = cell R.RightJustify
  
cell ::
  Ix c
  => R.Justification
  -> c
  -> C.TextSpec
  -> B.Table c (B.PostingInfo, B.Queried c)
  -> R.Cell
cell j c ts t = R.emptyCell j w ts where
  w = C.Width . B.unColumnWidth . widest t $ c  

cellWidth :: R.Cell -> B.ColumnWidth
cellWidth = B.ColumnWidth . C.unWidth . R.widestLine

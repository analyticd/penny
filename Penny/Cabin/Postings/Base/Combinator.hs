module Penny.Cabin.Postings.Base.Combinator where

import Data.Array (Ix)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Table as Ta

import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
import qualified Penny.Cabin.Colors as C

-- | Calculate the widest cell in a column.
widest ::
  Ix c
  => B.Table c (B.PostingInfo, B.Queried c)
  -> c
  -> C.Width
widest t c = F.foldr f (C.Width 0) col where
  col = Ta.OneDim $ Ta.column t c
  f (_, q) widestSoFar = case q of
    B.QGrowToFit (cw, _) ->
      max (C.Width . B.unColumnWidth $ cw) widestSoFar
    _ -> widestSoFar

lJustCell ::
  Ix c
  => c
  -> C.TextSpec
  -> Seq.Seq C.Chunk
  -> B.Table c (B.PostingInfo, B.Queried c)
  -> R.Cell
lJustCell = cell R.LeftJustify
  
rJustCell ::
  Ix c
  => c
  -> C.TextSpec
  -> Seq.Seq C.Chunk
  -> B.Table c (B.PostingInfo, B.Queried c)
  -> R.Cell
rJustCell = cell R.RightJustify
  
cell ::
  Ix c
  => R.Justification
  -> c
  -> C.TextSpec
  -> Seq.Seq C.Chunk
  -> B.Table c (B.PostingInfo, B.Queried c)
  -> R.Cell
cell j c ts ks t = R.Cell j w ts ks where
  w = widest t c  

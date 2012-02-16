module Penny.Cabin.Postings.Base.Combinator where

import qualified Control.Monad.Trans.State as St
import Data.Array (Array, Ix)
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty)
import Data.Map as Map
import qualified Data.Map as M
import qualified Data.Table as Ta
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Traversable as T
import Data.Word (Word)

import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Postings.Base.Row as R
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

-- | Allocate an integer evenly between the number of fractions
-- given. The allocation is not guaranteed to be exactly proportional,
-- but it is guaranteed to add up to the original integer given.
allocate :: NonEmpty Double -> Word -> NonEmpty Word
allocate ds w = let
  tot = F.sum ds
  ratios = fmap (/ tot) ds
  rounded = fmap (round . (* (fromIntegral w))) ratios
  in adjust rounded w

adjust :: NonEmpty Word -> Word -> NonEmpty Word
adjust ws w = let
  wsInts = fmap fromIntegral ws
  diff = (fromIntegral w) - F.sum wsInts in
  if diff == 0
  then ws
  else let
    ws' = St.evalState (T.mapM adjustMap ws) diff
    in adjust ws' w

-- | The state is the target number minus the current actual total.
adjustMap :: Word -> St.State Int Word
adjustMap w = do
  diff <- St.get
  case compare diff 0 of
    EQ -> return w
    GT -> do
      St.put (pred diff)
      return (succ w)
    LT -> do
      St.put (succ diff)
      return (pred w)

module Penny.Cabin.Postings.Row (
  Padding(Padding, unPadding),
  Cell(Cell),
  Row,
  emptyRow,
  prependCell,
  appendCell,
  Rows,
  emptyRows,
  prependRow,
  appendRow,
  HasChunk(chunk)) where

import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.Foldable as F
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S

import Penny.Cabin.Colors (Chunk)

newtype Padding = Padding { unPadding :: Chunk }

data Cell = Cell Padding (Seq Chunk) 

newtype Row = Row { unRow :: Seq Cell }

emptyRow :: Row
emptyRow = Row S.empty

prependCell :: Cell -> Row -> Row
prependCell c (Row cs) = Row (c <| cs) 

appendCell :: Cell -> Row -> Row
appendCell c (Row cs) = Row (cs |> c)

height :: Cell -> Int
height (Cell _ s) = S.length s

padCell :: Int -> Cell -> Cell
padCell i (Cell pa@(Padding p) s) = Cell pa (s `mappend` pads) where
  difference = i - S.length s
  pads =
    if difference > 0
    then S.replicate difference p
    else S.empty

padCells :: Seq Cell -> Seq Cell
padCells cs = fmap (padCell maxHeight) cs where
  maxHeight = F.maximum (fmap height cs)

newtype Rows = Rows { unRows :: Seq Row }

emptyRows :: Rows
emptyRows = Rows S.empty

prependRow :: Row -> Rows -> Rows
prependRow r (Rows rs) = Rows (r <| rs)

appendRow :: Row -> Rows -> Rows
appendRow r (Rows rs) = Rows (rs |> r)

instance Monoid Rows where
  mempty = Rows S.empty
  mappend (Rows s1) (Rows s2) = Rows $ mappend s1 s2

class HasChunk a where
  chunk :: a -> Chunk

instance HasChunk Row where
  chunk (Row cs) = cellsToChunk cs

instance HasChunk Rows where
  chunk (Rows rs) =
    F.foldr mappend mempty (fmap chunk rs)

cellsToChunk :: Seq Cell -> Chunk
cellsToChunk cells =
  if S.null cells
  then mempty
  else F.foldr mappend mempty zipped where
    zipped = F.foldr1 zipper padded
    padded = fmap (\(Cell _ cs) -> cs) (padCells cells)
    zipper s1 s2 = S.zipWith mappend s1 s2

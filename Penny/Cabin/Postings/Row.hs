module Penny.Cabin.Postings.Row (
  Padding(Padding, unPadding),
  Cell(Cell),
  Row,
  toRow,
  Rows,
  emptyRows,
  prependRow,
  appendRow) where

import Data.Monoid (Monoid, mempty, mappend, mconcat)
import qualified Data.Foldable as F
import Data.Sequence (Seq, (<|), (|>))
import qualified Data.Sequence as S

import Penny.Cabin.Colors (Chunk)

newtype Padding = Padding { unPadding :: Chunk }

data Cell = Cell Padding (Seq Chunk) 

data Row = Empty | Full (Seq Cell)

instance Monoid Row where
  mempty = Empty
  mappend Empty r = r
  mappend l Empty = l
  mappend (Full s1) (Full s2) = Full s' where
    s' = padCells (s1 `mappend` s2)

toRow :: Cell -> Row
toRow c = Full $ S.singleton c

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
  chunk Empty = mempty
  chunk (Full cells) =
    F.foldr mappend mempty (fmap (chunk . cellToColumn) cells)

instance HasChunk Rows where
  chunk (Rows rs) =
    F.foldr mappend mempty (fmap chunk rs)

instance HasChunk Column where
  chunk EmptyCol = mempty
  chunk (Column seq) = F.foldl mappend mempty seq

data Column = EmptyCol | Column (Seq Chunk)

cellToColumn :: Cell -> Column
cellToColumn (Cell _ cs) = Column cs

instance Monoid Column where
  mempty = EmptyCol
  mappend (Column s1) (Column s2) = Column $ S.zipWith mappend s1 s2

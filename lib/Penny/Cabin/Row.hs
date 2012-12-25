-- | Displays a single on-screen row. A row may contain multiple screen lines and multiple columns.
module Penny.Cabin.Row (
  Justification(LeftJustify, RightJustify),
  ColumnSpec(ColumnSpec, justification, width, padSpec, bits),
  C.Width(Width, unWidth),
  row ) where

import Data.List (transpose)
import qualified Data.Text as X
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Scheme as E

-- | How to justify cells. LeftJustify leaves the right side
-- ragged. RightJustify leaves the left side ragged.
data Justification =
  LeftJustify
  | RightJustify
  deriving Show

-- | A cell of text output. You tell the cell how to justify itself
-- and how wide it is. You also tell it the background colors to
-- use. The cell will be appropriately justified (that is, text
-- aligned between left and right margins) and padded (with lines of
-- blank text added on the bottom as needed) when joined with other
-- cells into a Row.
data ColumnSpec =
  ColumnSpec { justification :: Justification
             , width :: C.Width
             , padSpec :: (E.Label, E.EvenOdd)
             , bits :: [E.PreChunk] }

newtype JustifiedCell = JustifiedCell (Either (E.PreChunk, E.PreChunk)
                                              E.PreChunk)
data JustifiedColumn = JustifiedColumn {
  justifiedCells :: [JustifiedCell]
  , _justifiedWidth :: C.Width
  , _justifiedPadSpec :: (E.Label, E.EvenOdd) }

newtype PaddedColumns = PaddedColumns [[JustifiedCell]]
newtype CellsByRow = CellsByRow [[JustifiedCell]]
newtype CellRowsWithNewlines = CellRowsWithNewlines [[JustifiedCell]]


justify
  :: C.Width
  -> Justification
  -> E.PreChunk
  -> JustifiedCell
justify (C.Width w) j pc
  | origWidth < w = JustifiedCell . Left $ pair
  | otherwise = JustifiedCell . Right $ pc
    where
      origWidth = C.unWidth . E.width $ pc
      lbl = E.label pc
      eo = E.evenOdd pc
      pad = E.PreChunk lbl eo t
      t = X.replicate (w - origWidth) (X.singleton ' ')
      pair = case j of
        LeftJustify -> (pc, pad)
        RightJustify -> (pad, pc)

newtype Height = Height { _unHeight :: Int }
                 deriving (Show, Eq, Ord)

height :: [[a]] -> Height
height = Height . maximum . map length

row :: [ColumnSpec] -> [E.PreChunk]
row =
  concat
  . concat
  . toBits
  . toCellRowsWithNewlines
  . toCellsByRow
  . bottomPad
  . map justifiedColumn

justifiedColumn :: ColumnSpec -> JustifiedColumn
justifiedColumn (ColumnSpec j w ts bs) = JustifiedColumn cs w ts where
  cs = map (justify w j) $ bs

bottomPad :: [JustifiedColumn] -> PaddedColumns
bottomPad jcs = PaddedColumns pcs where
  justCells = map justifiedCells jcs
  (Height h) = height justCells
  pcs = map toPaddedColumn jcs
  toPaddedColumn (JustifiedColumn cs (C.Width w) (lbl, eo)) = let
    l = length cs
    nPads = h - l
    pad = E.PreChunk lbl eo t
    t = X.replicate w (X.singleton ' ')
    pads = replicate nPads . JustifiedCell . Right $ pad
    cs'
      | l < h = cs ++ pads
      | otherwise = cs
    in cs'


toCellsByRow :: PaddedColumns -> CellsByRow
toCellsByRow (PaddedColumns cs) = CellsByRow (transpose cs)


toCellRowsWithNewlines :: CellsByRow -> CellRowsWithNewlines
toCellRowsWithNewlines (CellsByRow bs) =
  CellRowsWithNewlines bs' where
    bs' = foldr f [] bs
    newline = JustifiedCell . Right
              $ E.PreChunk E.Other E.Even (X.singleton '\n')
    f cells acc = (cells ++ [newline]) : acc


toBits :: CellRowsWithNewlines -> [[[E.PreChunk]]]
toBits (CellRowsWithNewlines cs) = map (map toB) cs where
  toB (JustifiedCell c) = case c of
    Left (lb, rb) -> [lb, rb]
    Right b -> [b]


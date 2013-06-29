{-# LANGUAGE OverloadedStrings #-}
-- | Displays a single on-screen row. A row may contain multiple
-- screen lines and multiple columns.
--
-- This module only deals with a single row at a time. Each cell in
-- the row can have more than one screen line; this module will make
-- sure that the cells have appropriate padding on the bottom so that
-- the row appears nicely. This module will also justify each cell so
-- that its left side or right side is ragged; however, you first have
-- to specify how wide you want the cell to be.
--
-- This module is a little dumber than you might first think it could
-- be. For instance it would be possible to write a function that
-- takes a number of rows and automatically justifies all the cells by
-- finding the widest cell in a column. Indeed I might eventually
-- write such a function because it might be useful in, for example,
-- the multi-commodity balance report. However, such a function would
-- not be useful in all cases; in particular, the Posts report is very
-- complicated to lay out, and the automatic function described above
-- would not do the right thing.
--
-- So this module offers some useful automation, even if it is at a
-- level that is apparently lower that what is possible. Thus the
-- present 'row' function likely will not change, even if eventually I
-- add a 'table' function that automatically justifies many rows.
module Penny.Cabin.Row (
  Justification(LeftJustify, RightJustify),
  ColumnSpec(ColumnSpec, justification, width, padSpec, bits),
  Width(Width, unWidth),
  row ) where

import Data.List (transpose)
import Data.Monoid (mempty)
import qualified Data.Text as X
import qualified Penny.Cabin.Scheme as E
import qualified System.Console.Rainbow as R

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
             , width :: Width
             , padSpec :: (E.Label, E.EvenOdd)
             , bits :: [R.Chunk] }

newtype JustifiedCell = JustifiedCell (R.Chunk, R.Chunk)

data JustifiedColumn = JustifiedColumn {
  justifiedCells :: [JustifiedCell]
  , _justifiedWidth :: Width
  , _justifiedPadSpec :: (E.Label, E.EvenOdd) }

newtype PaddedColumns = PaddedColumns [[JustifiedCell]]
newtype CellsByRow = CellsByRow [[JustifiedCell]]
newtype CellRowsWithNewlines = CellRowsWithNewlines [[JustifiedCell]]
newtype Width = Width { unWidth :: Int }
  deriving (Eq, Ord, Show)

justify
  :: Width
  -> Justification
  -> E.Label
  -> E.EvenOdd
  -> E.Changers
  -> R.Chunk
  -> JustifiedCell
justify (Width w) j l eo chgrs pc = JustifiedCell (left, right)
  where
    origWidth = X.length . R._text $ pc
    pad = E.getEvenOddLabelValue l eo chgrs . R.Chunk mempty $ t
    t = X.replicate (max 0 (w - origWidth)) (X.singleton ' ')
    (left, right) = case j of
      LeftJustify -> (pc, pad)
      RightJustify -> (pad, pc)

newtype Height = Height Int
  deriving (Show, Eq, Ord)

height :: [[a]] -> Height
height xs = case xs of
  [] -> Height 0
  ls -> Height . maximum . map length $ ls

row :: E.Changers -> [ColumnSpec] -> [R.Chunk]
row chgrs =
  concat
  . concat
  . toBits
  . toCellRowsWithNewlines
  . toCellsByRow
  . bottomPad chgrs
  . map (justifiedColumn chgrs)

justifiedColumn :: E.Changers -> ColumnSpec -> JustifiedColumn
justifiedColumn chgrs (ColumnSpec j w (l, eo) bs)
  = JustifiedColumn cs w (l, eo)
  where
    cs = map (justify w j l eo chgrs) bs

bottomPad :: E.Changers -> [JustifiedColumn] -> PaddedColumns
bottomPad chgrs jcs = PaddedColumns pcs where
  justCells = map justifiedCells jcs
  (Height h) = height justCells
  pcs = map toPaddedColumn jcs
  toPaddedColumn (JustifiedColumn cs (Width w) (lbl, eo)) =
    let l = length cs
        nPads = max 0 $ h - l
        pad = E.getEvenOddLabelValue lbl eo chgrs . R.Chunk mempty $ t
        t = X.replicate w (X.singleton ' ')
        pads = replicate nPads $ JustifiedCell (mempty, pad)
    in cs ++ pads


toCellsByRow :: PaddedColumns -> CellsByRow
toCellsByRow (PaddedColumns cs) = CellsByRow (transpose cs)


toCellRowsWithNewlines :: CellsByRow -> CellRowsWithNewlines
toCellRowsWithNewlines (CellsByRow bs) =
  CellRowsWithNewlines bs' where
    bs' = foldr f [] bs
    newline = JustifiedCell (mempty, "\n")
    f cells acc = (cells ++ [newline]) : acc


toBits :: CellRowsWithNewlines -> [[[R.Chunk]]]
toBits (CellRowsWithNewlines cs) = map (map toB) cs where
  toB (JustifiedCell (c1, c2)) = [c1, c2]


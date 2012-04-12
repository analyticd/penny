module Penny.Cabin.Row (
  Justification(LeftJustify, RightJustify),
  ColumnSpec(ColumnSpec, justification, width, padSpec, bits),
  row ) where

import Data.List (transpose)
import qualified Data.Text as X
import qualified Penny.Cabin.Chunk as C

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
             , padSpec :: C.TextSpec
             , bits :: [C.Bit] }

newtype JustifiedCell = JustifiedCell (Either (C.Bit, C.Bit) C.Bit)
data JustifiedColumn = JustifiedColumn {
  justifiedCells :: [JustifiedCell]
  , _justifiedWidth :: C.Width
  , _justifiedPadSpec :: C.TextSpec }

data PaddedColumns = PaddedColumns {
  _paddedCells :: [[JustifiedCell]]
  , _paddedHeight :: Height }

newtype WithNewlineColumn = WithNewlineColumn [[JustifiedCell]]

justify ::
  C.TextSpec
  -> C.Width
  -> Justification
  -> C.Bit
  -> JustifiedCell
justify ts (C.Width w) j b
  | origWidth < w = JustifiedCell . Left $ pair
  | otherwise = JustifiedCell . Right $ b
    where
      origWidth = C.unWidth . C.bitWidth $ b
      pad = C.bit ts t
      t = X.replicate (w - origWidth) (X.singleton ' ')
      pair = case j of
        LeftJustify -> (b, pad)
        RightJustify -> (pad, b)

newtype Height = Height { _unHeight :: Int }
                 deriving (Show, Eq, Ord)

height :: [[a]] -> Height
height = Height . maximum . map length

row :: [ColumnSpec] -> [C.Bit]
row =
  concat
  . concat
  . toBits
  . withNewlineColumn
  . bottomPad
  . map justifiedColumn

justifiedColumn :: ColumnSpec -> JustifiedColumn
justifiedColumn (ColumnSpec j w ts bs) = JustifiedColumn cs w ts where
  cs = map (justify ts w j) $ bs

bottomPad :: [JustifiedColumn] -> PaddedColumns
bottomPad jcs = PaddedColumns pcs (Height h) where
  justCells = map justifiedCells jcs
  (Height h) = height justCells
  pcs = map toPaddedColumn jcs
  toPaddedColumn (JustifiedColumn cs (C.Width w) ts) = let
    l = length cs
    nPads = h - l
    pad = C.bit ts t
    t = X.replicate w (X.singleton ' ')
    pads = replicate nPads . JustifiedCell . Right $ pad
    cs'
      | l < h = cs ++ pads
      | otherwise = cs
    in cs'

withNewlineColumn :: PaddedColumns -> WithNewlineColumn
withNewlineColumn (PaddedColumns cs (Height h)) =
  WithNewlineColumn cs' where
    newlineList = replicate h newline
    newline = [JustifiedCell . Right
               $ C.bit C.defaultTextSpec (X.singleton '\n')]
    cs' = cs ++ newlineList

toBits :: WithNewlineColumn -> [[[C.Bit]]]
toBits (WithNewlineColumn cs) = map (map toB) . transpose $ cs where
  toB (JustifiedCell c) = case c of
    Left (lb, rb) -> [lb, rb]
    Right b -> [b]
    

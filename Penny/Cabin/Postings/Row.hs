module Penny.Cabin.Postings.Row (
  Justification(LeftJustify, RightJustify),
  Width(Width, unWidth),
  Cell,
  emptyCell,
  appendLine,
  prependLine,
  Row,
  emptyRow,
  prependCell,
  appendCell,
  Rows,
  emptyRows,
  prependRow,
  appendRow,
  HasChunk(chunk)) where

import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Foldable as F
import Data.Sequence (Seq, (<|), (|>), ViewL ((:<)))
import qualified Data.Sequence as S
import qualified Data.Text as X
import Data.Word (Word)

import Penny.Cabin.Colors (Chunk, Color8, Color256, text, chunkSize)

-- | How to justify cells. LeftJustify leaves the right side
-- ragged. RightJustify leaves the left side ragged.
data Justification =
  LeftJustify
  | RightJustify
  deriving Show

-- | How wide to make a cell. Each line within the cell is padded so
-- it is at least this wide. Lines that are longer than this are NOT
-- chopped or truncated; they are left as is. 
data Width = Width { unWidth :: Word }
             deriving Show

-- | A cell of text output. You tell the cell how to justify itself
-- and how wide it is. You also tell it the background colors to
-- use. The cell will be appropriately justified (that is, text
-- aligned between left and right margins) and padded (with lines of
-- blank text added on the bottom as needed) when joined with other
-- cells into a Row.
data Cell =
  Cell { justification :: Justification
       , width :: Width
       , padBackground :: (Color8, Color256)
       , chunks :: Seq Chunk }

data PaddedCell =
  PaddedCell { justifiedChunks :: Seq Chunk
             , _bottom :: Chunk }

emptyCell :: Justification -> Width -> Color8 -> Color256 -> Cell
emptyCell j w c8 c256 = Cell j w (c8, c256) S.empty

appendLine :: Chunk -> Cell -> Cell
appendLine ch c = c { chunks = chunks c |> ch }

prependLine :: Chunk -> Cell -> Cell
prependLine ch c = c { chunks = ch <| chunks c }

-- | A Row consists of several Cells. The Cells will be padded and
-- justified appropriately, with the padding adjusting to accomodate
-- other Cells in the Row.
newtype Row = Row (Seq PaddedCell)

emptyRow :: Row
emptyRow = Row S.empty

justify :: Color8 -> Color256 -> Width -> Justification -> Chunk -> Chunk
justify c8 c256 (Width w) j c = glue padding c where
  glue pd ck = case j of
    LeftJustify -> ck `mappend` pd
    RightJustify -> pd `mappend` ck
  padding = text c8 c256 t
  t = X.pack (replicate s ' ')
  s = if chunkSize c >= w then 0 else fromIntegral $ w - chunkSize c

newtype Height = Height Word
                 deriving Show

bottomPad ::
  Color8
  -> Color256
  -> Width
  -> Chunk
bottomPad c8 c256 (Width w) = text c8 c256 t where
  t = X.pack (replicate (fromIntegral w) ' ')

morePadding ::
  Height
  -> PaddedCell
  -> PaddedCell
morePadding (Height h) (PaddedCell cs c) = PaddedCell cs' c where
  cs' = if fromIntegral h > S.length cs
        then cs `mappend` pads
        else cs
  pads = S.replicate (fromIntegral h - S.length cs) c

justifyAll ::
  Color8 -> Color256 -> Width -> Justification -> Seq Chunk -> Seq Chunk
justifyAll c8 c256 w j = fmap (justify c8 c256 w j)

addCell ::
  (PaddedCell -> Seq PaddedCell -> Seq PaddedCell)
  -> Cell
  -> Row
  -> Row
addCell glue c (Row cs) = let
  (c8, c256) = padBackground c
  justified = justifyAll c8 c256 (width c)
              (justification c) (chunks c)
  pc = PaddedCell justified (bottomPad c8 c256 (width c))
  in case S.viewl cs of
    S.EmptyL -> Row (S.singleton pc)
    existing :< _ -> let
      pc' = morePadding (height existing) pc
      in Row (glue pc' cs)

prependCell :: Cell -> Row -> Row
prependCell = addCell (<|) 

appendCell :: Cell -> Row -> Row
appendCell = addCell (flip (|>))

height :: PaddedCell -> Height
height (PaddedCell cs _) = Height (fromIntegral (S.length cs))

-- | Several rows, joined together.
newtype Rows = Rows (Seq Row)

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
  chunk (Row cells) =
    if S.null cells
    then mempty
    else F.foldr mappend mempty zipped where
      zipped = F.foldr1 zipper (fmap justifiedChunks cells)
      zipper s1 s2 = S.zipWith mappend s1 s2

instance HasChunk Rows where
  chunk (Rows rs) = F.foldr mappend mempty (fmap chunk rs)

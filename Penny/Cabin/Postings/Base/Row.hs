module Penny.Cabin.Postings.Base.Row (
  Justification(LeftJustify, RightJustify),
  Cell(Cell, justification, width, padSpec, chunks),
  zeroCell,
  widestLine,
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
import Data.Sequence (Seq, (<|), (|>), ViewL ((:<)),
                      ViewR ((:>)))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as X

import Penny.Cabin.Colors
  (Chunk, TextSpec, chunkSize, Width(Width, unWidth))
import qualified Penny.Cabin.Colors as C

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
data Cell =
  Cell { justification :: Justification
       , width :: Width
       , padSpec :: TextSpec
       , chunks :: Seq Chunk }

data PaddedCell =
  PaddedCell { justifiedChunks :: Seq Chunk
             , _bottom :: Chunk }

-- | Creates a Cell that has no width. It will be padded on the bottom
-- as necessary, but because the cell has no width and no chunks, it
-- will never appear.
zeroCell :: Cell
zeroCell = Cell LeftJustify (Width 0) C.defaultSpec S.empty

widestLine :: Seq Chunk -> Width
widestLine = F.foldr max (Width 0) . fmap chunkSize

-- | A Row consists of several Cells. The Cells will be padded and
-- justified appropriately, with the padding adjusting to accomodate
-- other Cells in the Row.
newtype Row = Row (Seq PaddedCell)

emptyRow :: Row
emptyRow = Row S.empty

justify :: TextSpec -> Width -> Justification -> Chunk -> Chunk
justify ts wi@(Width w) j c = glue padding c where
  glue pd ck = case j of
    LeftJustify -> ck `mappend` pd
    RightJustify -> pd `mappend` ck
  padding = C.chunk ts t
  t = X.pack (replicate s ' ')
  s = if wi > chunkSize c
      then w - (unWidth . chunkSize $ c)
      else 0

newtype Height = Height { unHeight :: Int }
                 deriving (Show, Eq, Ord)

bottomPad :: TextSpec -> Width -> Chunk
bottomPad ts (Width w) = C.chunk ts t where
  t = X.pack (replicate w ' ')

morePadding ::
  Height
  -> PaddedCell
  -> PaddedCell
morePadding h (PaddedCell cs c) = PaddedCell cs' c where
  cs' = if unHeight h > S.length cs
        then cs `mappend` pads
        else cs
  pads = S.replicate (unHeight h - S.length cs) c

justifyAll ::
  TextSpec -> Width -> Justification -> Seq Chunk -> Seq Chunk
justifyAll ts w j = fmap (justify ts w j)

addCell ::
  (PaddedCell -> Seq PaddedCell -> Seq PaddedCell)
  -> Cell
  -> Row
  -> Row
addCell glue c (Row cs) = let
  s = padSpec c
  justified = justifyAll s (width c)
              (justification c) (chunks c)
  pc = PaddedCell justified (bottomPad s (width c))
  in case S.viewl cs of
    S.EmptyL -> Row (S.singleton pc)
    existing :< _ -> let
      newHeight = max (height existing) (height pc)
      pcs = glue pc cs
      in Row (fmap (morePadding newHeight) pcs)

prependCell :: Cell -> Row -> Row
prependCell = addCell (<|) 

appendCell :: Cell -> Row -> Row
appendCell = addCell (flip (|>))

height :: PaddedCell -> Height
height (PaddedCell cs _) = Height (S.length cs)

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

-- | Wraps a list of words into a list of lines, where each line is a
-- given maximum number of characters long. /This function is
-- partial/. It will call 'error' if the maximum number of characters
-- per line is less than 1.
--
-- An individual word will be split across multiple lines only if that
-- word is too long to fit into a single line.
{-
wordWrap :: Int -> Seq Text -> Seq Text
wordWrap l ts = sentences where
  maxLine = if l < 1
            then error "wordWrap: argument must be bigger than zero"
            else l
  sentences = F.foldl f ts S.empty
  f w sq =
    if X.length w > maxLine
    then let
      lenBot = case S.viewR sq of
        S.EmptyR -> Nothing
        _ :> t -> 
      cs = S.fromList $ X.chunksOf maxLine w
      in 
-}

data Words = Words (Seq X.Text) deriving Show
lenWords :: Words -> Int
lenWords (Words s) = case S.length s of
  0 -> 0
  l -> (F.sum . fmap X.length $ s) + (l - 1)

-- | Adds a word to a Words, but only if it will not make the Words
-- exceed the given length.
addWord :: Int -> Words -> X.Text -> Maybe Words
addWord l (Words ws) w =
  let words' = Words (ws |> w)
  in if lenWords words' > l
     then Nothing
     else Just words'

-- | Adds a word to a Words. If the word is too long to fit, breaks it
-- and adds the longest portion possible. Returns the new Words, and a
-- Text with the part of the word that was not added (if any; if all
-- of the word was added, return an empty Text.)
addPartialWord :: Int -> Words -> X.Text -> (Words, X.Text)
addPartialWord l (Words ws) t = case addWord l' (Words ws) t of
  (Just ws') -> (ws', X.empty)
  Nothing ->
    let maxChars = case S.length ws of
          0 -> l'
          x -> case l' of
            1 -> 0
            len -> l' - lenWords (Words ws) - 1
        (begin, end) = X.splitAt maxChars t
    in (Words (if X.null begin then ws else ws |> begin), end)
  where l' = if l < 0
             then error "addPartialWord error"
             else l

wordWrap :: F.Foldable f => Int -> f Text -> Seq Words
wordWrap = undefined

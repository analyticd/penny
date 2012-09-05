-- | Creates the output Chunks for the Balance report for both
-- multi-commodity and single-commodity reports.

module Penny.Cabin.Balance.Chunker (
  Columns(..),
  PreSpec(..),
  preSpecsToBits
  ) where

import Control.Applicative
  (Applicative (pure), (<$>), (<*>))
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Row as R
import qualified Data.Foldable as Fdbl

type IsEven = Bool

data Columns a = Columns {
  account :: a
  , drCr :: a
  , commodity :: a
  , quantity :: a
  } deriving Show

instance Functor Columns where
  fmap f c = Columns {
    account = f (account c)
    , drCr = f (drCr c)
    , commodity = f (commodity c)
    , quantity = f (quantity c)
    }

instance Applicative Columns where
  pure a = Columns a a a a
  fn <*> fa = Columns {
    account = (account fn) (account fa)
    , drCr = (drCr fn) (drCr fa)
    , commodity = (commodity fn) (commodity fa)
    , quantity = (quantity fn) (quantity fa)
     }

data PreSpec = PreSpec {
  justification :: R.Justification
  , padSpec :: Chunk.TextSpec
  , bits :: [Chunk.Chunk] }

-- | When given a list of columns, determine the widest row in each
-- column.
maxWidths :: [Columns PreSpec] -> Columns R.Width
maxWidths = Fdbl.foldl' maxWidthPerColumn (pure (R.Width 0))

-- | Applied to a Columns of PreSpec and a Colums of widths, return a
-- Columns that has the wider of the two values.
maxWidthPerColumn ::
  Columns R.Width
  -> Columns PreSpec
  -> Columns R.Width
maxWidthPerColumn w p = f <$> w <*> p where
  f old new = max old (maximum . map Chunk.chunkWidth . bits $ new)
  
-- | Changes a single set of Columns to a set of ColumnSpec of the
-- given width.
preSpecToSpec ::
  Columns R.Width
  -> Columns PreSpec
  -> Columns R.ColumnSpec
preSpecToSpec ws p = f <$> ws <*> p where
  f width (PreSpec j ps bs) = R.ColumnSpec j width ps bs

resizeColumnsInList :: [Columns PreSpec] -> [Columns R.ColumnSpec]
resizeColumnsInList cs = map (preSpecToSpec w) cs where
  w = maxWidths cs


-- Step 9
widthSpacerAcct :: Int
widthSpacerAcct = 4

widthSpacerDrCr :: Int
widthSpacerDrCr = 1

widthSpacerCommodity :: Int
widthSpacerCommodity = 1

colsToBits ::
  IsEven
  -> C.BaseColors
  -> Columns R.ColumnSpec
  -> [Chunk.Chunk]
colsToBits isEven bc (Columns a dc c q) = let
  fillSpec = if isEven
             then C.evenColors bc
             else C.oddColors bc
  spacer w = R.ColumnSpec j (Chunk.Width w) fillSpec []
  j = R.LeftJustify
  cs = a
       : spacer widthSpacerAcct
       : dc
       : spacer widthSpacerDrCr
       : c
       : spacer widthSpacerCommodity
       : q
       : []
  in R.row cs

colsListToBits ::
  C.BaseColors
  -> [Columns R.ColumnSpec]
  -> [[Chunk.Chunk]]
colsListToBits bc = zipWith f bools where
  f b c = colsToBits b bc c
  bools = iterate not True

preSpecsToBits ::
  C.BaseColors
  -> [Columns PreSpec]
  -> [Chunk.Chunk]
preSpecsToBits bc =
  concat
  . colsListToBits bc
  . resizeColumnsInList

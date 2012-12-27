-- | Creates the output Chunks for the Balance report for
-- multi-commodity reports only.

module Penny.Cabin.Balance.Convert.Chunker (
  MainRow(..),
  OneColRow(..),
  Row(..),
  rowsToChunks
  ) where


import Control.Applicative
  (Applicative (pure), (<$>), (<*>))
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Meta as Meta
import qualified Penny.Cabin.Row as R
import qualified Penny.Lincoln as L
import qualified Data.Foldable as Fdbl
import qualified Data.Text as X

type IsEven = Bool

data Columns a = Columns {
  acct :: a
  , drCr :: a
  , quantity :: a
  } deriving Show

instance Functor Columns where
  fmap f c = Columns {
    acct = f (acct c)
    , drCr = f (drCr c)
    , quantity = f (quantity c)
    }

instance Applicative Columns where
  pure a = Columns a a a
  fn <*> fa = Columns {
    acct = (acct fn) (acct fa)
    , drCr = (drCr fn) (drCr fa)
    , quantity = (quantity fn) (quantity fa)
     }

data PreSpec = PreSpec {
  _justification :: R.Justification
  , _padSpec :: (E.Label, E.EvenOdd)
  , bits :: E.PreChunk }

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
  f old new = max old (E.width . bits $ new)
  
-- | Changes a single set of Columns to a set of ColumnSpec of the
-- given width.
preSpecToSpec ::
  Columns R.Width
  -> Columns PreSpec
  -> Columns R.ColumnSpec
preSpecToSpec ws p = f <$> ws <*> p where
  f width (PreSpec j ps bs) = R.ColumnSpec j width ps [bs]

resizeColumnsInList :: [Columns PreSpec] -> [Columns R.ColumnSpec]
resizeColumnsInList cs = map (preSpecToSpec w) cs where
  w = maxWidths cs


widthSpacerAcct :: Int
widthSpacerAcct = 4

widthSpacerDrCr :: Int
widthSpacerDrCr = 1

colsToBits ::
  IsEven
  -> Columns R.ColumnSpec
  -> [E.PreChunk]
colsToBits isEven (Columns a dc q) = let
  fillSpec = if isEven
             then (E.Other, E.Even)
             else (E.Other, E.Odd)
  spacer w = R.ColumnSpec j (Chunk.Width w) fillSpec []
  j = R.LeftJustify
  cs = a
       : spacer widthSpacerAcct
       : dc
       : spacer widthSpacerDrCr
       : q
       : []
  in R.row cs

colsListToBits
  :: [Columns R.ColumnSpec]
  -> [[E.PreChunk]]
colsListToBits = zipWith f bools where
  f b c = colsToBits b c
  bools = iterate not True

preSpecsToBits
  :: [Columns PreSpec]
  -> [E.PreChunk]
preSpecsToBits =
  concat
  . colsListToBits
  . resizeColumnsInList

data Row = RMain MainRow | ROneCol OneColRow

-- | Displays a one-column row. 
data OneColRow = OneColRow {
  ocIndentation :: Int
  -- ^ Indent the text by this many levels (not by this many
  -- spaces; this number is multiplied by another number in the
  -- Chunker source to arrive at the final indentation amount)

  , ocText :: X.Text
  -- ^ Text for the left column
  }

-- | Displays a single account in a Balance report. In a
-- single-commodity report, this account will only be one screen line
-- long. In a multi-commodity report, it might be multiple lines long,
-- with one screen line for each commodity.
data MainRow = MainRow {
  mrIndentation :: Int
  -- ^ Indent the account name by this many levels (not by this many
  -- spaces; this number is multiplied by another number in the
  -- Chunker source to arrive at the final indentation amount)

  , mrText :: X.Text
  -- ^ Text for the name of the account

  , mrBottomLine :: L.BottomLine
  -- ^ Commodity balances. If this list is empty, dashes are
  -- displayed for the DrCr and Qty.
  }


rowsToChunks ::
  (L.Qty -> X.Text)
  -- ^ How to format a balance to allow for digit grouping
  -> [Row]
  -> [E.PreChunk]
rowsToChunks fmt =
  preSpecsToBits
  . rowsToColumns fmt

rowsToColumns ::
  (L.Qty -> X.Text)
  -- ^ How to format a balance to allow for digit grouping

  -> [Row]
  -> [Columns PreSpec]
rowsToColumns fmt rs = map (mkRow fmt) pairs
  where
    pairs = Meta.visibleNums (,) rs


mkRow ::
  (L.Qty -> X.Text)
  -> (Meta.VisibleNum, Row)
  -> Columns PreSpec
mkRow fmt (vn, r) = case r of
  RMain m -> mkMainRow fmt (vn, m)
  ROneCol c -> mkOneColRow (vn, c)

mkOneColRow ::
  (Meta.VisibleNum, OneColRow)
  -> Columns PreSpec
mkOneColRow (vn, (OneColRow i t)) = Columns ca cd cq
  where
    txt = X.append indents t
    indents = X.replicate (indentAmount * max 0 i)
              (X.singleton ' ')
    eo = E.fromVisibleNum vn
    lbl = E.Other
    ca = PreSpec R.LeftJustify (lbl, eo) (E.PreChunk lbl eo txt)
    cd = PreSpec R.LeftJustify (lbl, eo) (E.PreChunk lbl eo X.empty)
    cq = cd

mkMainRow ::
  (L.Qty -> X.Text)
  -> (Meta.VisibleNum, MainRow)
  -> Columns PreSpec
mkMainRow fmt (vn, (MainRow i acctTxt b)) = Columns ca cd cq
  where
    eo = E.fromVisibleNum vn
    lbl = E.Other
    ca = PreSpec R.LeftJustify (lbl, eo) (E.PreChunk lbl eo txt)
      where
        txt = X.append indents acctTxt
        indents = X.replicate (indentAmount * max 0 i)
                  (X.singleton ' ')
    cd = PreSpec R.LeftJustify (lbl, eo) cksDrCr
    cq = PreSpec R.LeftJustify (lbl, eo) cksQty
    (cksDrCr, cksQty) = balanceChunks fmt vn b


balanceChunks ::
  (L.Qty -> X.Text)
  -> Meta.VisibleNum
  -> L.BottomLine
  -> (E.PreChunk, E.PreChunk)
balanceChunks fmt vn bl = (chkDc, chkQt)
  where
    eo = E.fromVisibleNum vn
    chkDc = E.bottomLineToDrCr bl eo
    chkQt = E.PreChunk lbl eo t
      where
        (lbl, t) = case bl of
          L.Zero -> (E.Zero, X.pack "--")
          L.NonZero (L.Column dc qt) -> (E.dcToLbl dc, fmt qt)


indentAmount :: Int
indentAmount = 2


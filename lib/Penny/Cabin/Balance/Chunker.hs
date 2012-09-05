-- | Creates the output Chunks for the Balance report for both
-- multi-commodity and single-commodity reports.

module Penny.Cabin.Balance.Chunker (
  Row(..),
  rowsToChunks
  ) where


import Control.Applicative
  (Applicative (pure), (<$>), (<*>))
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Meta as Meta
import qualified Penny.Cabin.Row as R
import qualified Penny.Lincoln as L
import qualified Data.Foldable as Fdbl
import qualified Data.Text as X

type IsEven = Bool

data Columns a = Columns {
  acct :: a
  , drCr :: a
  , commodity :: a
  , quantity :: a
  } deriving Show

instance Functor Columns where
  fmap f c = Columns {
    acct = f (acct c)
    , drCr = f (drCr c)
    , commodity = f (commodity c)
    , quantity = f (quantity c)
    }

instance Applicative Columns where
  pure a = Columns a a a a
  fn <*> fa = Columns {
    acct = (acct fn) (acct fa)
    , drCr = (drCr fn) (drCr fa)
    , commodity = (commodity fn) (commodity fa)
    , quantity = (quantity fn) (quantity fa)
     }

data PreSpec = PreSpec {
  _justification :: R.Justification
  , _padSpec :: Chunk.TextSpec
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

-- | Displays a single account in a Balance report. In a
-- single-commodity report, this account will only be one screen line
-- long. In a multi-commodity report, it might be multiple lines long,
-- with one screen line for each commodity.
data Row = Row {
  indentation :: Int
  -- ^ Indent the account name by this many levels (not by this many
  -- spaces; this number is multiplied by another number in the
  -- Chunker source to arrive at the final indentation amount)

  , accountTxt :: X.Text
    -- ^ Text for the name of the account

  , balances :: [(L.Commodity, L.BottomLine)]
    -- ^ Commodity balances. If this list is empty, dashes are
    -- displayed for the DrCr, Commodity, and Qty.
  }

rowsToChunks ::
  (L.Commodity -> L.Qty -> X.Text)
  -- ^ How to format a balance to allow for digit grouping
  -> C.DrCrColors
  -> C.BaseColors
  -> [Row]
  -> [Chunk.Chunk]
rowsToChunks fmt dc b =
  preSpecsToBits b
  . rowsToColumns fmt dc b

rowsToColumns ::
  (L.Commodity -> L.Qty -> X.Text)
  -- ^ How to format a balance to allow for digit grouping

  -> C.DrCrColors
  -> C.BaseColors
  -> [Row]
  -> [Columns PreSpec]
rowsToColumns fmt dc bc rs = map (mkColumn fmt dc bc) pairs
  where
    pairs = Meta.visibleNums (,) rs


mkColumn ::
  (L.Commodity -> L.Qty -> X.Text)
  -> C.DrCrColors
  -> C.BaseColors
  -> (Meta.VisibleNum, Row)
  -> Columns PreSpec
mkColumn fmt dc bc (vn, (Row i acctTxt bs)) = Columns ca cd cc cq
  where
    baseCol = C.colors vn bc
    ca = PreSpec R.LeftJustify baseCol [Chunk.chunk baseCol txt]
      where
        txt = X.append indents acctTxt
        indents = X.replicate (indentAmount * max 0 i)
                  (X.singleton ' ')
    cd = PreSpec R.LeftJustify baseCol cksDrCr
    cc = PreSpec R.RightJustify baseCol cksCmdty
    cq = PreSpec R.LeftJustify baseCol cksQty
    (cksDrCr, cksCmdty, cksQty) =
      if null bs
      then balanceChunksEmpty dc vn
      else
        let balChks = map (balanceChunks fmt dc vn) bs
            cDrCr = map (\(a, _, _) -> a) balChks
            cCmdty = map (\(_, a, _) -> a) balChks
            cQty = map (\(_, _, a) -> a) balChks
        in (cDrCr, cCmdty, cQty)


balanceChunksEmpty ::
  C.DrCrColors
  -> Meta.VisibleNum
  -> ([Chunk.Chunk], [Chunk.Chunk], [Chunk.Chunk])
balanceChunksEmpty dc vn = (dash, dash, dash)
  where
    ts = C.colors vn . C.bottomLineToBaseColors dc $ L.Zero
    dash = [Chunk.chunk ts (X.pack "--")]

balanceChunks ::
  (L.Commodity -> L.Qty -> X.Text)
  -> C.DrCrColors
  -> Meta.VisibleNum
  -> (L.Commodity, L.BottomLine)
  -> (Chunk.Chunk, Chunk.Chunk, Chunk.Chunk)
balanceChunks fmt dcCol vn (cty, bl) = (chkDc, chkCt, chkQt)
  where
    ts = C.colors vn . C.bottomLineToBaseColors dcCol $ bl
    chk = Chunk.chunk ts
    (dcTxt, qtyTxt) = case bl of
      L.Zero -> (X.pack "--", X.pack "--")
      L.NonZero (L.Column dc q) ->
        let dx = case dc of
              L.Debit -> X.pack "Dr"
              L.Credit -> X.pack "Cr"
            qx = fmt cty q
        in (dx, qx)
    chkDc = chk dcTxt
    chkQt = chk qtyTxt
    chkCt = chk (L.text $ L.Delimited (X.singleton ':') (L.textList cty))


indentAmount :: Int
indentAmount = 2

-- | Takes postings and places them into a tree for further
-- processing.
--
-- Steps:
--
-- 1. [LT.PostingInfo] -> RawBals
-- 2. RawBals -> (SummedBals, TotalBal)
-- 3. (SummedBals, TotalBal) -> (SummedWithIsEven, TotalBal)
-- 4. (SummedWithIsEven, TotalBal) -> (PreSpecMap, TotalBal)
-- 5. (PreSpecMap, TotalBal) -> [Columns PreSpec]
-- 6. [Columns PreSpec] -> [Columns R.ColumnSpec] (strict)
-- 7. [Columns R.ColumnSpec] -> [R.Row] (lazy)
-- 8. [R.Row] -> XL.Text (lazy)
module Penny.Cabin.Balance.Tree (report) where

import Control.Applicative(Applicative(pure, (<*>)), (<$>))
import qualified Control.Monad.Trans.State as St
import qualified Penny.Cabin.Row as R
import Penny.Cabin.Row((|>>))
import qualified Data.Sequence as Seq
import Data.Sequence ((|>))
import qualified Data.Foldable as Fdbl
import qualified Data.Functor.Identity as Id
import qualified Data.Map as M
import qualified Data.Monoid as Monoid
import qualified Penny.Cabin.Balance.NestedMap as NM
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import qualified Data.Traversable as Tr
import qualified Penny.Cabin.Balance.Options as O
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C
import qualified Penny.Liberty.Types as LT
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Balance as Bal
import qualified Data.Semigroup as S

data PreSpec = PreSpec {
  justification :: R.Justification
  , padSpec :: Chunk.TextSpec
  , bits :: [Chunk.Bit] }

-- Step 1
newtype RawBal = RawBal { unRawBal :: S.Option Bal.Balance }
instance Monoid.Monoid RawBal where
  mappend (RawBal b1) (RawBal b2) = RawBal $ b1 `Monoid.mappend` b2
  mempty = RawBal Monoid.mempty

type RawBals = NM.NestedMap L.SubAccountName RawBal

-- Step 2
newtype SummedBal =
  SummedBal { unSummedBal :: S.Option Bal.Balance }
instance Monoid.Monoid SummedBal where
  mappend (SummedBal b1) (SummedBal b2) =
    SummedBal $ b1 `Monoid.mappend` b2
  mempty = SummedBal Monoid.mempty
type TotalBal = SummedBal
type SummedBals = NM.NestedMap L.SubAccountName SummedBal

-- Step 3
type SummedWithIsEven = NM.NestedMap L.SubAccountName (SummedBal, Bool)

-- Step 4
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

data Columns a = Columns {
  account :: a
  , drCr :: a
  , commodity :: a
  , quantity :: a
  } deriving Show

type PreSpecMap = NM.NestedMap L.SubAccountName (Columns PreSpec)

-- Step 5

-- Step 6

-- Step 7

-- Step 8

report :: O.Options -> [LT.PostingInfo] -> XL.Text
report os =
  rowsToChunk
  . columnListToRows os
  . resizeColumnsInList
  . makeColumnList os
  . makeCellsInMap os
  . makeSummedWithIsEven
  . sumBalances
  . rawBalances

rowsToChunk :: [R.Row] -> Chunk.Chunk
rowsToChunk = R.chunk . Fdbl.foldl' R.appendRow R.emptyRows

columnListToRows :: O.Options -> [Columns R.Cell] -> [R.Row]
columnListToRows os = zipWith f bools where
  f b c = cellsToRow os b c
  bools = iterate not True

resizeColumnsInList :: [Columns R.Cell] -> [Columns R.Cell]
resizeColumnsInList cs = resize mw cs where
  mw = Fdbl.foldl' f (pure 0) cs where
    maxCol old new =
      max old (Chunk.unWidth . R.widestLine . R.chunks $ new)
    f acc cols = maxCol <$> acc <*> cols
  resize widths = fmap resizeRow where
    resizeCell w c = c { R.width = Chunk.Width w }
    resizeRow = (resizeCell <$> widths <*> )


makeColumnList :: O.Options -> (CellsInMap, TotalBal) -> [Columns R.Cell]
makeColumnList os (cim, tb) = totCols : restCols where
  totCols = makeTotalCells os tb
  restCols = Fdbl.toList cim

makeCellsInMap ::
  O.Options
  -> (SummedWithIsEven, TotalBal)
  -> (CellsInMap, TotalBal)
makeCellsInMap os (sb, tb) = (cim, tb) where
  cim = Id.runIdentity (NM.traverseWithTrail (traverser os) sb)

makeSummedWithIsEven ::
  (SummedBals, TotalBal)
  -> (SummedWithIsEven, TotalBal)
makeSummedWithIsEven (sb, tb) = (swie, tb) where
  swie = St.evalState (Tr.mapM f sb) False
  f lbl = do
    st <- St.get
    St.put (not st)
    return (lbl, st)

sumBalances :: RawBals -> (SummedBals, TotalBal)
sumBalances rb = (sb, (SummedBal . unRawBal $ tb)) where
  (tb, rawBals) = NM.cumulativeTotal rb
  sb = fmap (SummedBal . unRawBal) rawBals

rawBalances :: [LT.PostingInfo] -> RawBals
rawBalances = Fdbl.foldl' addPosting NM.empty . map toPair where
  toPair p = let
    box = LT.postingBox p
    ac = Q.account box
    en = Q.entry box
    in (ac, en)


-- | Inserts a single posting into the Balances tree.
addPosting :: RawBals -> (L.Account, L.Entry) -> RawBals
addPosting bals (ac, en) = let
  bal = RawBal . S.Option . Just . L.entryToBalance $ en
  subs = Fdbl.toList . L.unAccount $ ac
  in NM.insert bals subs bal

type IsEven = Bool

widthSpacerAcct :: Int
widthSpacerAcct = 4

widthSpacerDrCr :: Int
widthSpacerDrCr = 1

widthSpacerCommodity :: Int
widthSpacerCommodity = 1

cellsToRow :: O.Options -> IsEven -> Columns R.Cell -> R.Row
cellsToRow os isEven (Columns a dc c q) = let
  fillSpec = if isEven
             then C.evenColors . O.baseColors $ os
             else C.oddColors . O.baseColors $ os
  spacer w = R.Cell j (Chunk.Width w) fillSpec Seq.empty
  j = R.LeftJustify
  in R.emptyRow
     |>> a
     |>> spacer widthSpacerAcct
     |>> dc
     |>> spacer widthSpacerDrCr
     |>> c
     |>> spacer widthSpacerCommodity
     |>> q


traverser ::
  O.Options
  -> [(L.SubAccountName, (SummedBal, Bool))]
  -> L.SubAccountName
  -> (SummedBal, Bool)
  -> a
  -> Id.Identity (Maybe (Columns R.Cell))
traverser os hist a mayBal _ =
  return (Just $ makeCells os hist a mayBal)


makeTotalCells ::
  O.Options
  -> SummedBal
  -> Columns R.Cell
makeTotalCells os mayBal = Columns act dc com qt where
  act = accountCell os True 0 tot
  tot = L.SubAccountName $ L.TextNonEmpty 'T' (X.pack "otal")
  (dc, com, qt) = bottomLineCells os True mayBal

makeCells ::
  O.Options
  -> [(L.SubAccountName, (SummedBal, Bool))]
  -> L.SubAccountName
  -> (SummedBal, Bool)
  -> Columns R.Cell
makeCells os ps a (mayBal, isEven) = Columns act dc com qt where
  lvl = length ps + 1
  act = accountCell os isEven lvl a
  (dc, com, qt) = bottomLineCells os isEven mayBal

fillTextSpec ::
  O.Options
  -> IsEven
  -> Chunk.TextSpec
fillTextSpec os isEven = let
  getTs = if isEven then C.evenColors else C.oddColors
  in getTs . O.baseColors $ os
  

padding :: Int
padding = 2

accountCell ::
  O.Options
  -> IsEven
  -> Int
  -> L.SubAccountName
  -> R.Cell
accountCell os isEven lvl acct = R.Cell j w ts chk where
  j = R.LeftJustify
  w = Chunk.Width 0
  ts = if isEven
       then C.evenColors . O.baseColors $ os
       else C.oddColors . O.baseColors $ os
  chk = Seq.singleton $ Chunk.chunk ts txt where
    txt = pad `X.append` (L.text acct)
    pad = X.replicate (padding * lvl) (X.singleton ' ')

bottomLineCells ::
  O.Options
  -> IsEven
  -> SummedBal
  -> (R.Cell, R.Cell, R.Cell)
bottomLineCells os isEven mayBal = let
  fill = fillTextSpec os isEven
  tsZero = if isEven
           then C.evenZero . O.drCrColors $ os
           else C.oddZero . O.drCrColors $ os
  zeroCell =
    R.Cell R.LeftJustify (Chunk.Width 0)
    tsZero (Seq.singleton (Chunk.chunk tsZero (X.pack "--")))
  zeroCells = (zeroCell, zeroCell, zeroCell)
  in case S.getOption . unSummedBal $ mayBal of
    Nothing -> zeroCells
    Just bal -> bottomLineBalCells fill
                . map (bottomLineBalChunks os isEven)
                . M.assocs
                . Bal.unBalance
                $ bal


-- | Takes a list of triples from bottomLineChunks and creates three
-- Cells, one each for DrCr, Commodity, and Qty.
bottomLineBalCells ::
  Chunk.TextSpec -- ^ Fill colors
  -> [(Chunk.Chunk, Chunk.Chunk, Chunk.Chunk)]
  -> (R.Cell, R.Cell, R.Cell)
bottomLineBalCells spec ts = (mkCell dc, mkCell ct, mkCell qt) where
  mkCell sq = R.Cell R.LeftJustify (Chunk.Width 0) spec sq
  e = Seq.empty
  (dc, ct, qt) = Fdbl.foldl' f (e, e, e) ts
  f (da, ca, qa) (d, c, q) = (da |> d, ca |> c, qa |> q)

-- | Returns a triple (x, y, z), where x is the DrCr chunk, y is the
-- commodity chunk, and z is the qty chunk.
bottomLineBalChunks ::
  O.Options
  -> IsEven
  -> (L.Commodity, Bal.BottomLine)
  -> (Chunk.Chunk, Chunk.Chunk, Chunk.Chunk)
bottomLineBalChunks os isEven (comm, bl) = (dc, cty, qty) where
  dc = Chunk.chunk ts dcTxt
  cty = Chunk.chunk ts ctyTxt
  qty = Chunk.chunk ts qtyTxt
  ctyTxt = L.text (L.Delimited (X.singleton ':') (L.textList comm))
  (ts, dcTxt, qtyTxt) = case bl of
    Bal.Zero -> let
      getTs = if isEven then C.evenZero else C.oddZero
      dcT = X.pack "--"
      qtyT = dcT
      in (getTs . O.drCrColors $ os, dcT, qtyT)
    Bal.NonZero clm -> let
      (getTs, dcT) = case Bal.drCr clm of
        L.Debit ->
          (if isEven then C.evenDebit else C.oddDebit,
           X.pack "Dr")
        L.Credit ->
          (if isEven then C.evenCredit else C.oddCredit,
           X.pack "Cr")
      qTxt = (O.balanceFormat os) bl
      in (getTs . O.drCrColors $ os, dcT, qTxt)

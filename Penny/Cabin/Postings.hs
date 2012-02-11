module Penny.Cabin.Postings where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Foldable (toList)
import qualified Data.Foldable as F
import Data.List (zipWith)
import Data.List.NonEmpty (NonEmpty, toNonEmpty)
import qualified Data.List.ZipNonEmpty as ZNE
import Data.Map (Map)
import Data.Monoid (mempty, mappend, Monoid)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Table (
  Table, table, changeColumns, RowNum, ColNum,
  changeRows)
import Data.Traversable (mapAccumL)
import Data.Word (Word)

import Penny.Lincoln.Balance (Balance, entryToBalance)
import Penny.Lincoln.Boxes (PostingBox, PriceBox)
import Penny.Lincoln.Queries (entry)

import Penny.Cabin.Colors (Chunk)

data Allocation = Allocation { unAllocation :: Double }
                  deriving Show

data ColumnWidth = ColumnWidth { unColumnWidth :: Word }
                   deriving Show

data ReportWidth = ReportWidth { unReportWidth :: Word }
                   deriving Show

data CellInfo =
  CellInfo { cellPosting :: PostingBox
           , cellBalance :: Balance
           , cellRow :: RowNum
           , cellCol :: ColNum }

type PadderF = ColumnWidth -> Chunk

type GrowF =
  ReportWidth
  -> [PriceBox]
  -> CellInfo
  -> (ColumnWidth, PadderF, Map RowNum Queried -> [Chunk])

type AllocateF =
  ReportWidth
  -> [PriceBox]
  -> CellInfo
  -> Map ColNum Expanded
  -> (PadderF, [Chunk])

data Column =
  GrowToFit GrowF
  | Allocate Allocation AllocateF

data Columns = Columns { unColumns :: NonEmpty Column }

data Queried =
  EGrowToFit (ColumnWidth, PadderF, Map RowNum Queried -> [Chunk])
  | EAllocate Allocation
    (Map ColNum Expanded -> (PadderF, [Chunk]))

data Expanded =
  Grown (PadderF, [Chunk])
  | ExAllocate Allocation
    (Map ColNum Expanded -> (PadderF, [Chunk]))

balanceAccum :: Balance -> PostingBox -> (Balance, Balance)
balanceAccum bal pb = (bal', bal') where
  bal' = bal `mappend` pstgBal
  pstgBal = entryToBalance . entry $ pb

balances :: NonEmpty PostingBox
            -> NonEmpty Balance
balances = snd . mapAccumL balanceAccum mempty

balanceAndPostings :: NonEmpty PostingBox
                      -> NonEmpty (PostingBox, Balance)
balanceAndPostings pbs = 
  ZNE.ne
  $ pure (,)
  <*> ZNE.zipNe pbs
  <*> ZNE.zipNe (balances pbs)
                         
postingsTable ::
  ReportWidth
  -> Columns
  -> [PriceBox]
  -> [PostingBox]
  -> Maybe (Table (PadderF, [Chunk]))
postingsTable rw cols prices pstgs = do
  nePstgs <- toNonEmpty pstgs
  return $
    makeTable rw cols prices (balanceAndPostings nePstgs)

makeTable ::
  ReportWidth
  -> Columns
  -> [PriceBox]
  -> NonEmpty (PostingBox, Balance)
  -> Table (PadderF, [Chunk])
makeTable rw cols prices =
  allocate
  . expand
  . fmap (queried rw prices)
  . cellInfos
  . paired cols

paired ::
  Columns
  -> NonEmpty (PostingBox, Balance)
  -> Table ((PostingBox, Balance), Column)
paired (Columns cs) ps = table (,) ps cs

cellInfos ::
  Table ((PostingBox, Balance), Column)
  -> Table (CellInfo, Column)
cellInfos = changeRows f where
  f rn cn _ ((pb, bal), col) = (CellInfo pb bal rn cn, col)

queried ::
  ReportWidth
  -> [PriceBox]
  -> (CellInfo, Column)
  -> Queried
queried rw pbs (ci, col) = case col of
  GrowToFit f -> EGrowToFit $ f rw pbs ci
  Allocate a f -> EAllocate a $ f rw pbs ci

expand :: Table Queried -> Table Expanded
expand = changeColumns f where
  f _ _ rowMap q = case q of
    EGrowToFit (_, padder, grower) -> Grown (padder, grower rowMap)
    EAllocate a f -> ExAllocate a f

allocate :: Table Expanded -> Table (PadderF, [Chunk])
allocate = changeRows f where
  f _ _ colMap e = case e of
    Grown cs -> cs
    ExAllocate a f -> f colMap

--
-- Filling in the lines
--

fillLines ::
  (F.Foldable f, Monoid m)
  => f (Seq m)
  -> m
fillLines fdbl = fillLines' 0 fdbl mempty

fillLines' ::
  (F.Foldable f, Monoid m)
  => Int
  -> f (Seq m)
  -> m
  -> m
fillLines' i fdbl mLeft = case fillLine i fdbl of
  Nothing -> mLeft
  (Just m) -> fillLines' (succ i) fdbl (mLeft `mappend` m)

fillLine ::
  (F.Foldable f, Monoid m)
  => Int
  -> f (Seq m)
  -> Maybe m
fillLine i = F.foldlM f mempty where
  f m s = case safeIndex s i of
    (Just piece) -> Just $ m `mappend` piece
    Nothing -> Nothing

safeIndex :: Seq a -> Int -> Maybe a
safeIndex s i = if i < S.length s
                then Just $ S.index s i
                else Nothing

newtype ChunkCell = ChunkCell { unChunkCell :: [Chunk] }

instance Monoid ChunkCell where
  mempty = ChunkCell $ repeat mempty
  mappend (ChunkCell ls) (ChunkCell rs) =
    ChunkCell $ zipWith mappend ls rs

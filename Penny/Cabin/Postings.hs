module Penny.Cabin.Postings where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, toNonEmpty)
import qualified Data.List.ZipNonEmpty as ZNE
import Data.Map (Map)
import Data.Monoid (mempty, mappend)
import Data.Table (
  Table, table, changeColumns, RowNum, ColNum,
  changeRows)
import Data.Traversable (mapAccumL)
import Data.Word (Word)

import Penny.Lincoln.Balance (Balance, entryToBalance)
import Penny.Lincoln.Boxes (PostingBox, PriceBox)
import Penny.Lincoln.Queries (entry)

import Penny.Cabin.Colors (Chunk)

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

data Allocation = Allocation { unAllocation :: Double }
                  deriving Show

data ColumnWidth = ColumnWidth { unColumnWidth :: Word }
                   deriving Show

data ReportWidth = ReportWidth { unReportWidth :: Word }
                   deriving Show

type PadderF = ColumnWidth -> Chunk

data CellInfo =
  CellInfo { cellPosting :: PostingBox
           , cellBalance :: Balance
           , cellRow :: RowNum
           , cellCol :: ColNum }

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

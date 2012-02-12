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
import Penny.Cabin.Postings.Row (Cell)

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

type GrowF =
  ReportWidth
  -> [PriceBox]
  -> CellInfo
  -> (ColumnWidth, Map RowNum Queried -> Cell)

type AllocateF =
  ReportWidth
  -> [PriceBox]
  -> CellInfo
  -> Map ColNum Expanded
  -> Cell

data Column =
  GrowToFit GrowF
  | Allocate Allocation AllocateF

data Columns = Columns { unColumns :: NonEmpty Column }

data Queried =
  EGrowToFit (ColumnWidth, Map RowNum Queried -> Cell)
  | EAllocate Allocation
    (Map ColNum Expanded -> Cell)

data Expanded =
  Grown Cell
  | ExAllocate Allocation
    (Map ColNum Expanded -> Cell)

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
  -> Maybe (Table Cell)
postingsTable rw cols prices pstgs = do
  nePstgs <- toNonEmpty pstgs
  return $
    makeTable rw cols prices (balanceAndPostings nePstgs)

makeTable ::
  ReportWidth
  -> Columns
  -> [PriceBox]
  -> NonEmpty (PostingBox, Balance)
  -> Table Cell
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
    EGrowToFit (_, grower) -> Grown (grower rowMap)
    EAllocate a f -> ExAllocate a f

allocate :: Table Expanded -> Table Cell
allocate = changeRows f where
  f _ _ colMap e = case e of
    Grown cs -> cs
    ExAllocate a f -> f colMap

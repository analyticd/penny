module Penny.Cabin.Postings where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Foldable (toList)
import qualified Data.Foldable as F
import Data.List (zipWith)
import Data.List.NonEmpty (NonEmpty, toNonEmpty, unsafeToNonEmpty,
                           nonEmpty)
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

newtype PostingNum = PostingNum { unPostingNum :: Integer }
                     deriving (Show, Eq, Ord)

data CellInfo =
  CellInfo { cellRow :: RowNum
           , cellCol :: ColNum }

data PostingInfo =
  PostingInfo { postingNum :: PostingNum
              , balance :: Balance
              , postingBox :: PostingBox }

type GrowF =
  ReportWidth
  -> [PriceBox]
  -> PostingInfo
  -> CellInfo
  -> (ColumnWidth, Map RowNum Queried -> Cell)

type AllocateF =
  ReportWidth
  -> [PriceBox]
  -> PostingInfo
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
                      -> NonEmpty PostingInfo
balanceAndPostings pbs = 
  ZNE.ne
  $ pure PostingInfo
  <*> ZNE.zipNe (pure PostingNum <*> (nonEmpty 0 [1..]))
  <*> ZNE.zipNe (balances pbs)
  <*> ZNE.zipNe pbs
                         
postingsTable ::
  (PostingInfo -> Bool)
  -> ReportWidth
  -> Columns
  -> [PriceBox]
  -> [PostingBox]
  -> Maybe (Table Cell)
postingsTable p rw cols prices pstgs = do
  nePstgs <- toNonEmpty pstgs
  return $
    makeTable rw cols prices (balanceAndPostings nePstgs)

makeTable ::
  ReportWidth
  -> Columns
  -> [PriceBox]
  -> NonEmpty PostingInfo
  -> Table Cell
makeTable rw cols prices =
  allocate
  . expand
  . fmap (queried rw prices)
  . cellInfos
  . paired cols

paired ::
  Columns
  -> NonEmpty PostingInfo
  -> Table (PostingInfo, Column)
paired (Columns cs) ps = table (,) ps cs

cellInfos ::
  Table (PostingInfo, Column)
  -> Table (PostingInfo, CellInfo, Column)
cellInfos = changeRows f where
  f rn cn _ (pi, col) = (pi, (CellInfo rn cn), col)

queried ::
  ReportWidth
  -> [PriceBox]
  -> (PostingInfo, CellInfo, Column)
  -> Queried
queried rw pbs (pi, ci, col) = case col of
  GrowToFit f -> EGrowToFit $ f rw pbs pi ci
  Allocate a f -> EAllocate a $ f rw pbs pi ci

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

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
  -> Maybe (Table [Chunk])
postingsTable rw cols prices pstgs = do
  nePstgs <- toNonEmpty pstgs
  return $
    makeTable rw cols prices (balanceAndPostings nePstgs)

makeTable ::
  ReportWidth
  -> Columns
  -> [PriceBox]
  -> NonEmpty (PostingBox, Balance)
  -> Table [Chunk]
makeTable rw cols prices =
  allocate rw
  . expand
  . fmap (queried prices)
  . paired cols

data Allocation = Allocation { unAllocation :: Double }
                  deriving Show

data ColumnWidth = ColumnWidth { unColumnWidth :: Word }
                   deriving Show

data ReportWidth = ReportWidth { unReportWidth :: Word }
                   deriving Show

type GrowF =
  Balance
  -> PostingBox
  -> [PriceBox]
  -> (ColumnWidth, Map RowNum Queried -> [Chunk])

type AllocateF =
  Balance
  -> PostingBox
  -> [PriceBox]
  -> ReportWidth
  -> Map ColNum Expanded
  -> ColNum
  -> [Chunk]

data Column =
  GrowToFit GrowF
  | Allocate Allocation AllocateF

data Columns = Columns { unColumns :: NonEmpty Column }

{-
1. Ask all GrowToFit cells for their ColumnWidth and function.
2. Expand all GrowToFit cells.
3. Grow the GrowToFit cells.
4. For each row, compute allocation for Allocation cells; expand
Allocation cells.
-}

data Queried =
  EGrowToFit (ColumnWidth, Map RowNum Queried -> [Chunk])
  | EAllocate Allocation
    (ReportWidth -> Map ColNum Expanded -> ColNum -> [Chunk])


data Expanded =
  Grown [Chunk]
  | ExAllocate Allocation
    (ReportWidth -> Map ColNum Expanded -> ColNum -> [Chunk])

paired ::
  Columns
  -> NonEmpty (PostingBox, Balance)
  -> Table ((PostingBox, Balance), Column)
paired (Columns cs) ps = table (,) ps cs

queried :: [PriceBox] -> ((PostingBox, Balance), Column) -> Queried
queried pr ((pb, bal), c) = case c of
  GrowToFit f -> EGrowToFit $ f bal pb pr
  Allocate a f -> EAllocate a $ f bal pb pr

expand :: Table Queried -> Table Expanded
expand = changeColumns f where
  f colNum rowMap rowNum q = case q of
    EGrowToFit (w, grower) -> Grown $ grower rowMap
    EAllocate a f -> ExAllocate a f

allocate :: ReportWidth -> Table Expanded -> Table [Chunk]
allocate w = changeRows f where
  f rowNum colMap colNum e = case e of
    Grown cs -> cs
    ExAllocate a f -> f w colMap colNum

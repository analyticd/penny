module Penny.Cabin.Postings.Base where

import Control.Applicative (pure, (<*>))
import Data.Array (Array, Ix)
import qualified Data.Array as A
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, toNonEmpty, unsafeToNonEmpty,
                           nonEmpty)
import qualified Data.List.ZipNonEmpty as ZNE
import Data.Monoid (mempty, mappend, Monoid)
import Data.Traversable (mapAccumL)
import Data.Word (Word)

import qualified Penny.Cabin.ArrayHelpers as H
import Penny.Cabin.Colors (Chunk)
import Penny.Cabin.Postings.Row
  (Cell, Row, Rows, emptyRow, prependCell,
   emptyRows, prependRow, chunk)
import Penny.Lincoln.Balance (Balance, entryToBalance)
import Penny.Lincoln.Boxes (PostingBox, PriceBox)
import Penny.Lincoln.Queries (entry)

newtype RowNum = RowNum { unRowNum :: Word }
                 deriving (Eq, Ord, Show, A.Ix)


data Allocation = Allocation { unAllocation :: Double }
                  deriving Show

newtype ColumnWidth = ColumnWidth { unColumnWidth :: Int }
                   deriving (Show, Eq, Ord)

data ReportWidth = ReportWidth { unReportWidth :: Int }
                   deriving (Show, Eq, Ord)

newtype PostingNum = PostingNum { unPostingNum :: Int }
                     deriving (Show, Eq, Ord)

data CellInfo c =
  CellInfo { cellCol :: c
           , cellRow :: RowNum }

data PostingInfo =
  PostingInfo { postingNum :: PostingNum
              , balance :: Balance
              , postingBox :: PostingBox }

type Table c e = Array (c, RowNum) e

type GrowF c =
  ReportWidth
  -> [PriceBox]
  -> PostingInfo
  -> CellInfo c
  -> (ColumnWidth, Table c (Queried c) -> Cell)

data Queried c =
  EGrowToFit (ColumnWidth, Table c (Queried c) -> Cell)
  | EAllocate Allocation
    (Table c (Expanded c) -> Cell)

data Expanded c =
  Grown Cell
  | ExAllocate Allocation
    (Table c (Expanded c) -> Cell)

type AllocateF c =
  ReportWidth
  -> [PriceBox]
  -> PostingInfo
  -> CellInfo c
  -> Table c (Expanded c)
  -> Cell

data Column c =
  GrowToFit (GrowF c)
  | Allocate Allocation (AllocateF c)

data Columns c = Columns { unColumns :: NonEmpty (Column c) }

newtype RowsPerPosting =
  RowsPerPosting { unRowsPerPosting :: Int }
  deriving Show

rowsPerPosting :: Int -> RowsPerPosting
rowsPerPosting i =
  if i < 1
  then error "rowsPerPosting: must have at least 1 row per posting"
  else RowsPerPosting i

allocation :: Double -> Allocation
allocation d =
  if d > 0
  then Allocation d
  else error "allocations must be greater than zero"

balanceAccum :: Balance -> PostingBox -> (Balance, Balance)
balanceAccum bal pb = (bal', bal') where
  bal' = bal `mappend` pstgBal
  pstgBal = entryToBalance . entry $ pb

balances :: NonEmpty PostingBox
            -> NonEmpty Balance
balances = snd . mapAccumL balanceAccum mempty

postingInfos :: NonEmpty PostingBox
                -> NonEmpty PostingInfo
postingInfos pbs = 
  ZNE.ne
  $ pure PostingInfo
  <*> ZNE.zipNe (pure PostingNum <*> (nonEmpty 0 [1..]))
  <*> ZNE.zipNe (balances pbs)
  <*> ZNE.zipNe pbs
                         
tableToChunk :: Ix c => Table c Cell -> Chunk
tableToChunk = chunk . tableToRows

tableToRows :: Ix c => Table c Cell -> Rows
tableToRows = foldr prependRow emptyRows
              . map tableRowToRow
              . A.elems
              . H.rows

tableRowToRow :: Ix c => Array c Cell -> Row
tableRowToRow = foldr prependCell emptyRow . A.elems

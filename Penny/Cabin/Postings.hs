module Penny.Cabin.Postings where

import Data.List.NonEmpty (NonEmpty, toNonEmpty)
import Data.Table (Table, table, changeColumns)
import Data.Word (Word)

import Penny.Lincoln.Balance (Balance)
import Penny.Lincoln.Boxes (PostingBox, PriceBox)

import Penny.Cabin.Colors (Chunk)

data Allocation = Allocation { unAllocation :: Double }
                  deriving Show

data ColumnWidth = ColumnWidth { unColumnWidth :: Word }
                   deriving Show

type GrowF =
  Balance
  -> PostingBox
  -> [PriceBox]
  -> (ColumnWidth, ColumnWidth -> [Chunk])

type AllocateF =
  ColumnWidth
  -> Balance
  -> PostingBox
  -> [PriceBox]
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
  EGrowToFit (ColumnWidth, ColumnWidth -> [Chunk])
  | EAllocate Balance PostingBox [PriceBox] Allocation
    AllocateF

data Expanded =
  Grown [Chunk]
  | ExAllocate Balance PostingBox [PriceBox] Allocation
    AllocateF

paired ::
  Columns
  -> [(PostingBox, Balance)]
  -> Maybe (Table ((PostingBox, Balance), Column))
paired (Columns cs) pbs = case toNonEmpty pbs of
  Nothing -> Nothing
  (Just ps) -> Just $ table (,) ps cs

queried :: [PriceBox] -> ((PostingBox, Balance), Column) -> Queried
queried pr ((pb, bal), c) = case c of
  GrowToFit f -> EGrowToFit $ f bal pb pr
  Allocate a f -> EAllocate bal pb pr a f

expand :: Table Queried -> Table Expanded
expand = changeColumns f where

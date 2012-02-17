module Penny.Cabin.Postings.Base.Base where

import Control.Applicative
  (pure, (<*>), (<$>), ZipList(ZipList, getZipList))
import Control.Monad (filterM)
import qualified Control.Monad.Trans.State as St
import Data.Array (Array, Ix)
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Monoid (mempty, mappend, Monoid)
import qualified  Data.Table as Ta
import qualified Data.Traversable as T

import Penny.Cabin.Colors (Chunk)
import Penny.Cabin.Postings.Base.Row
  (Cell, emptyRow, prependCell,
   emptyRows, prependRow, chunk)
import Penny.Lincoln.Balance (Balance, entryToBalance)
import Penny.Lincoln.Boxes (PostingBox, PriceBox)
import Penny.Lincoln.Queries (entry)

newtype RowNum = RowNum { unRowNum :: Int }
                 deriving (Eq, Ord, Show, A.Ix)


data Allocation = Allocation { unAllocation :: Double }
                  deriving Show

newtype ColumnWidth = ColumnWidth { unColumnWidth :: Int }
                   deriving (Show, Eq, Ord)

data ReportWidth = ReportWidth { unReportWidth :: Int }
                   deriving (Show, Eq, Ord)

newtype PostingNum = PostingNum { unPostingNum :: Int }
                     deriving (Show, Eq, Ord)

newtype RevPostingNum =
  RevPostingNum { unRevPostingNum :: Int }
  deriving (Show, Eq, Ord)


newtype VisibleNum = VisibleNum { unVisibleNum :: Int }
                     deriving (Show, Eq, Ord)

data CellInfo c =
  CellInfo { cellCol :: c
           , cellRow :: RowNum }

data PostingInfo =
  PostingInfo { postingNum :: PostingNum
              , balance :: Balance
              , postingBox :: PostingBox 
              , visibleNum :: VisibleNum
              , revPostingNum :: RevPostingNum }

type Table c e = Array (c, RowNum) e

type GrowF c =
  ReportWidth
  -> [PriceBox]
  -> PostingInfo
  -> CellInfo c
  -> (ColumnWidth, Table c (PostingInfo, Queried c) -> Cell)

data Queried c =
  QGrowToFit (ColumnWidth, Table c (PostingInfo, Queried c) -> Cell)
  | QAllocate Allocation
    (Table c (PostingInfo, Expanded c) -> Cell)

data Expanded c =
  EGrown Cell
  | EAllocate Allocation
    (Table c (PostingInfo, Expanded c) -> Cell)

type AllocateF c =
  ReportWidth
  -> [PriceBox]
  -> PostingInfo
  -> CellInfo c
  -> Table c (PostingInfo, Expanded c)
  -> Cell

data Formula c =
  FGrowToFit (GrowF c)
  | FAllocate Allocation (AllocateF c)

data Columns c =
  Columns { unColumns :: Array c (Formula c) }

newtype RowsPerPosting =
  RowsPerPosting { unRowsPerPosting :: Int }
  deriving Show

allocation :: Double -> Allocation
allocation d =
  if d > 0
  then Allocation d
  else error "allocations must be greater than zero"

balanceAccum :: Balance -> PostingBox -> (Balance, Balance)
balanceAccum bal pb = (bal', bal') where
  bal' = bal `mappend` pstgBal
  pstgBal = entryToBalance . entry $ pb

balances :: [PostingBox]
            -> [Balance]
balances = snd . T.mapAccumL balanceAccum mempty

postingTriples ::
  [PostingBox]
  -> [(PostingBox, PostingNum, Balance)]
postingTriples pbs =
  getZipList
  $ (,,)
  <$> ZipList pbs
  <*> ZipList (PostingNum <$> [0..])
  <*> ZipList (balances pbs)

postingQuads :: [(PostingBox, PostingNum, Balance)]
                -> [(PostingBox, PostingNum, Balance, RevPostingNum)]
postingQuads ls =
  reverse
  $ getZipList
  $ (\(pb, pn, ba) rpn -> (pb, pn, ba, rpn))
  <$> ZipList (reverse ls)
  <*> ZipList (RevPostingNum <$> [0..])

quadsToInfos ::
  [(PostingBox, PostingNum, Balance, RevPostingNum)]
  -> [PostingInfo]
quadsToInfos ls = let
  makeInfo (pb, pn, ba, rpn) vn = PostingInfo pn ba pb vn rpn in
  getZipList
  $ makeInfo
  <$> ZipList ls
  <*> ZipList (VisibleNum <$> [0..])
  
type PostingPredicate =
  (PostingBox, PostingNum, Balance, RevPostingNum)
   -> Bool

postingInfos :: PostingPredicate
                -> [PostingBox]
                -> [PostingInfo]
postingInfos pp =
  quadsToInfos
  . filter pp
  . postingQuads
  . postingTriples

tableToChunk :: Ix c => Array (c, RowNum) Cell -> Chunk
tableToChunk =
  chunk
  . F.foldr prependRow emptyRows
  . Ta.OneDim
  . fmap (F.foldr prependCell emptyRow . Ta.OneDim)
  . Ta.rows

baseArray ::
  Ix c
  => RowsPerPosting
  -> [PostingInfo]
  -> Columns c
  -> Array (c, RowNum) (PostingInfo, Formula c)
baseArray (RowsPerPosting rpp) ps (Columns cs) = let
  (minC, maxC) = A.bounds cs
  rows = concatMap (replicate rpp) ps
  (minR, maxR) = (RowNum 0, RowNum (length rows - 1))
  is = A.range ((minC, minR), (maxC, maxR))
  columns = A.elems cs
  vs = flip (,)
       <$> columns
       <*> rows
  values = zip is vs
  in A.array ((minC, minR), (maxC, maxR)) values

cellInfos ::
  Ix c
  => Array (c, RowNum) (PostingInfo, Formula c)
  -> Array (c, RowNum) (CellInfo c, PostingInfo, Formula c)
cellInfos = fmap triple . label where
  label arr = A.array (A.bounds arr) ls where
    ls = fmap f . A.assocs $ arr
    f (i, v) = (i, (i, v))
  triple ((c, r), (p, f)) = (CellInfo c r, p, f)

queried ::
  ReportWidth
  -> [PriceBox]
  -> (CellInfo c, PostingInfo, Formula c)
  -> (PostingInfo, Queried c)
queried w ps (ci, p, fmla) = (p, q) where
  q = case fmla of
    FGrowToFit f -> QGrowToFit $ f w ps p ci
    FAllocate a f -> QAllocate a $ f w ps p ci

expanded ::
  Table c (PostingInfo, Queried c)
  -> (PostingInfo, Queried c)
  -> (PostingInfo, Expanded c)
expanded t (p, q) = (p, e) where
  e = case q of
    QGrowToFit (_, f) -> EGrown $ f t
    QAllocate a f -> EAllocate a f

allocate ::
  Table c (PostingInfo, Expanded c)
  -> (PostingInfo, Expanded c)
  -> Cell
allocate t (_, e) = case e of
  EGrown c -> c
  EAllocate _ f -> f t

infosToQueried ::
  Ix c
  => ReportWidth
  -> [PriceBox]
  -> Array (c, RowNum) (CellInfo c, PostingInfo, Formula c)
  -> Array (c, RowNum) (PostingInfo, Queried c)
infosToQueried w ps = fmap (queried w ps)

queriedToExpanded ::
  Ix c
  => Array (c, RowNum) (PostingInfo, Queried c)
  -> Array (c, RowNum) (PostingInfo, Expanded c)
queriedToExpanded a = fmap (expanded a) a

expandedToCells ::
  Ix c
  => Array (c, RowNum) (PostingInfo, Expanded c)
  -> Array (c, RowNum) Cell
expandedToCells a = fmap (allocate a) a

makeChunk ::
  Ix c
  => RowsPerPosting
  -> [PostingInfo]
  -> ReportWidth
  -> [PriceBox]
  -> Columns c
  -> Chunk
makeChunk rpp pis w pbs =
  tableToChunk
  . expandedToCells
  . queriedToExpanded
  . infosToQueried w pbs
  . cellInfos
  . baseArray rpp pis

report ::
  Ix c
  => RowsPerPosting
  -> PostingPredicate
  -> ReportWidth
  -> Columns c
  -> [PriceBox]
  -> [PostingBox]
  -> Chunk
report rpp pp rw c prs pos = makeChunk rpp pis rw prs c where
  pis = postingInfos pp pos

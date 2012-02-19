module Penny.Cabin.Postings.Grid where

import Control.Applicative (
  (<$>), (<*>), ZipList(ZipList, getZipList))
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Monoid (mempty, mappend)
import qualified Data.Table as Ta
import qualified Data.Traversable as Tr

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Row as R
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Boxes as B
import qualified Penny.Lincoln.Queries as Q

fmapArray ::
  A.Ix i
  => (A.Array i y -> i -> y -> z)
  -> A.Array i y
  -> A.Array i z
fmapArray f a = A.array b ls' where
  b = A.bounds a
  ls' = map f' . A.assocs $ a
  f' (i, e) = (i, f a i e)

-- Step 1 - Compute balances
balanceAccum :: Bal.Balance -> B.PostingBox -> (Bal.Balance, Bal.Balance)
balanceAccum bal pb = (bal', bal') where
  bal' = bal `mappend` pstgBal
  pstgBal = Bal.entryToBalance . Q.entry $ pb

balances :: [B.PostingBox] -> [(B.PostingBox, Bal.Balance)]
balances ps = zip ps (snd . Tr.mapAccumL balanceAccum mempty $ ps)

-- Step 2 - Number postings
numberPostings ::
  [(B.PostingBox, Bal.Balance)]
  -> [T.PostingInfo]
numberPostings ls = reverse reversed where
  withPostingNums =
    getZipList
    $ (\(pb, bal) pn -> (pb, bal, pn))
    <$> ZipList ls
    <*> ZipList (map T.PostingNum [0..])
  reversed =
    getZipList
    $ (\(pb, bal, pn) rpn -> T.PostingInfo pb bal pn rpn)
    <$> ZipList (reverse withPostingNums)
    <*> ZipList (map T.RevPostingNum [0..])

-- Step 3 - Get visible postings only
filterToVisible ::
  (T.PostingInfo -> Bool)
  -> [T.PostingInfo]
  -> [T.PostingInfo]
filterToVisible b ps = filter b ps  

-- Step 4 - add visible numbers
addVisibleNum ::
  [T.PostingInfo]
  -> [(T.PostingInfo, T.VisibleNum)]
addVisibleNum ls = zip ls (map T.VisibleNum [0..])

-- Step 5 - multiply into tranches
tranches ::
  Bounded t
  => [(T.PostingInfo, T.VisibleNum)]
  -> [(T.PostingInfo, T.VisibleNum, t)]
tranches ls =
  (\(p, vn) t -> (p, vn, t))
  <$> ls
  <*> [minBound, maxBound]

-- Step 6 - multiply to array
toArray ::
  (Bounded c, Bounded t, A.Ix c, A.Ix t)
  => [(T.PostingInfo, T.VisibleNum, t)]
  -> Maybe (A.Array (c, (T.VisibleNum, t)) T.PostingInfo)
toArray ls =
  if null ls
  then Nothing
  else let
    (_, maxVn, _) = last ls
    b = ((minBound, (T.VisibleNum 0, minBound)),
         (maxBound, (maxVn, maxBound)))
    pair c (p, vn, t) = ((c, (vn, t)), p)
    ps = pair
         <$> A.range (minBound, maxBound)
         <*> ls
    in Just $ A.array b ps

type Index c t = (c, (T.VisibleNum, t))

-- | Step 7 - Space claim. What different cells should do at this phase:
--
-- * Grow to fit cells - color coded blue. These should supply a Just
-- ClaimedWidth whose number indicates how wide their content will
-- be. They should do this only if their respective field is showing
-- in the final report; if the field is not showing, supply
-- Nothing. Also, if their field has no data to show (for instance,
-- this is a Flag field, and the posting has no flag), supply Nothing.
--
-- * Padding cells - these are color coded orange. They should supply
-- a Just ClaimedWidth, but only if their respective field is showing
-- and that field has data to show. In that circumstance, supply Just
-- (ClaimedWidth 1). Otherwise, supply Nothing.
--
-- * All other cells - supply Nothing.
type Claimer c t =
  A.Array (Index c t) T.PostingInfo
  -> Index c t
  -> T.PostingInfo
  -> Maybe T.ClaimedWidth

spaceClaim ::
  (A.Ix c, A.Ix t)
  => Claimer c t
  -> A.Array (Index c t) T.PostingInfo
  -> A.Array (Index c t) (T.PostingInfo, Maybe T.ClaimedWidth)
spaceClaim f = fmapArray g where
  g a i p = (p, f a i p)

-- | Step 8 - GrowToFit. What different cells should do at this phase:
--
-- * Grow to fit cells - these are color coded blue. These should
-- supply their actual data, and justify themselves to be as wide as
-- the widest cell in the column.
--
-- * Padding cells - these are color coded orange. These should supply
-- a cell that is justified to be as wide as the widest cell in the
-- column. Otherwise these cells contain no text at all (the Row
-- module takes care of supplying the necessary bottom padding lines
-- if they are needed.)
--
-- * Empty but padded cells - these are color coded yellow. Treat
-- these exactly the same as Padding cells.
--
-- * Overran cells - these are color coded light green. These should
-- supply Just 'Penny.Cabin.Row.zeroCell'.
--
-- * Allocated cells - these are color coded purple. These should
-- supply Nothing.
--
-- * Overrunning cells - these should supply Nothing.
type Grower c t =
  A.Array (Index c t) (T.PostingInfo, Maybe T.ClaimedWidth)
  -> Index c t
  -> (T.PostingInfo, Maybe T.ClaimedWidth)
  -> Maybe R.Cell

growCells ::
  (A.Ix c, A.Ix t)
  => Grower c t
  -> A.Array (Index c t) (T.PostingInfo, Maybe T.ClaimedWidth)
  -> A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
growCells f = fmapArray g where
  g a i (p, w) = (p, f a i (p, w))

-- | Step 9 - Allocate. What to do at this phase:
--
-- * GrowToFit, Padding, Empty, and Overran cells - have already
-- supplied a cell. Pass that cell along.
--
-- * Allocated cells - If the field is not selected to be in the
-- report, supply Nothing. If the field is selected, then use the
-- minimum report width, the width of all other GrowToFit and Padding
-- cells in the row, and the share allocated to other Allocated cells
-- that are going to show in the report. Supply a cell that is
-- justified to be exactly the necessary width. (The Row module will
-- not truncate or wrap cells, so the function must take care of this
-- on its own.)
--
-- * Overrunning cells - supply Nothing.
type Allocator c t =
  A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
  -> Index c t
  -> (T.PostingInfo, Maybe R.Cell)
  -> Maybe R.Cell

allocateCells ::
  (A.Ix c, A.Ix t)
  => Allocator c t
  -> A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
  -> A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
allocateCells f = fmapArray g where
  g a i (p, w) = (p, f a i (p, w))

-- | Step 10. Finalize all cells, including overruns. What cells should
-- do at this phase:
--
-- * GrowToFit, Empty but padded, padding, overran, allocated cells -
-- these have already supplied a cell. Pass this cell along.
--
-- * Overrunning cells - If the corresponding report is showing,
-- supply 'Penny.Cabin.Row.emptyCell'. If the field is showing,
-- calculate the width of the cell by using the width of the
-- appropriate cells in the top tranche row. Supply a cell that is
-- justified to exactly the correct width. (the Row module will not
-- truncate or wrap cells, so the function must do this itself.)
type Finalizer c t =
  A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
  -> Index c t
  -> (T.PostingInfo, Maybe R.Cell)
  -> R.Cell

finalize ::
  (A.Ix c, A.Ix t)
  => Finalizer c t
  -> A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
  -> CellArray c t
finalize f = CellArray . fmapArray f

-- Step 11 - make chunks
newtype CellArray c t =
  CellArray { unCellArray :: A.Array (Index c t) R.Cell }

instance (A.Ix c, A.Ix t) => R.HasChunk (CellArray c t) where
  chunk (CellArray a) = R.chunk rows where
    rows = F.foldr R.prependRow R.emptyRows rs
    rs = fmap toRow . Ta.OneDim . Ta.rows $ a
    toRow = F.foldr R.prependCell R.emptyRow

-- Put it all together!

report ::
  (A.Ix c, A.Ix t, Bounded c, Bounded t)
  => (T.PostingInfo -> Bool)
  -> Claimer c t
  -> Grower c t
  -> Allocator c t
  -> Finalizer c t
  -> [B.PostingBox]
  -> Maybe C.Chunk
report p c g a f pbs =
  (toArray
   . tranches
   . addVisibleNum
   . filterToVisible p
   . numberPostings
   . balances
   $ pbs )
  >>=
  (return
   . R.chunk
   . finalize f
   . allocateCells a
   . growCells g
   . spaceClaim c)
  

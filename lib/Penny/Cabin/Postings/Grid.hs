-- | The Postings grid.
--
-- The Postings report is just a big grid. This is represented as an
-- array. Filling in the grid is a multiple-step process. This module
-- contains the higher-level functions that are responsible for
-- filling in the grid.
--
-- The grid is cooperative: in order for things to line up on screen,
-- it is essential that each cell be the right size. However the model
-- ultimately relies on the contents of each cell to size itself
-- correctly, rather than other functions resizing the cells. This is
-- because ultimately each cell knows best how to size itself to
-- fit--for example, how it might truncate its contents to fit a
-- narrow cell.
module Penny.Cabin.Postings.Grid where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Array as A
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Table as Ta
import qualified Data.Traversable as Tr

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Row as R
import qualified Penny.Liberty.Types as LT
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Queries as Q

-- | fmap over an array, with additional information. Similar to fmap,
-- but with an ordinary fmap the function sees only the contents of
-- the current cell. fmapArray shows the function the entire array and
-- the address of the current cell, as well as the contents of the
-- cell itself. The function then returns the contents of the new
-- cell.
fmapArray ::
  A.Ix i
  => (A.Array i y -> i -> y -> z)
  -> A.Array i y
  -> A.Array i z
fmapArray f a = A.array b ls' where
  b = A.bounds a
  ls' = map f' . A.assocs $ a
  f' (i, e) = (i, f a i e)

-- * Step 1 - Compute balances

balanceAccum :: Maybe Bal.Balance
                -> LT.PostingInfo
                -> (Maybe Bal.Balance, (LT.PostingInfo, Bal.Balance))
balanceAccum mb po = (Just bal', (po, bal')) where
  bal' = let
    balThis = Bal.entryToBalance . Q.entry . LT.postingBox $ po
    in case mb of
      Nothing -> balThis
      Just balOld -> Bal.addBalances balOld balThis

balances :: NE.NonEmpty LT.PostingInfo
            -> NE.NonEmpty (LT.PostingInfo, Bal.Balance)
balances = snd . Tr.mapAccumL balanceAccum Nothing

-- * Step 2 - Number postings

numberPostings ::
  NE.NonEmpty (LT.PostingInfo, Bal.Balance)
  -> NE.NonEmpty T.PostingInfo
numberPostings ls = NE.reverse reversed where
  withPostingNums = NE.zipWith f ls ns where
    f (li, bal) pn = (li, bal, pn)
    ns = fmap T.PostingNum (NE.iterate succ 0)
  reversed = NE.zipWith f wpn rpns where
    f (li, bal, pn) rpn = T.fromLibertyInfo bal pn rpn li
    wpn = NE.reverse withPostingNums
    rpns = fmap T.RevPostingNum (NE.iterate succ 0)
    
-- * Step 3 - Get visible postings only

filterToVisible ::
  (LT.PostingInfo -> Bool) -- ^ Main predicate
  -> ([T.PostingInfo] -> [T.PostingInfo]) -- ^ Post filter
  -> NE.NonEmpty T.PostingInfo
  -> [T.PostingInfo]
filterToVisible p pf = pf . NE.filter p' where
  p' pstg = p (T.toLibertyInfo pstg)

-- * Step 4 - add visible numbers
addVisibleNum ::
  [T.PostingInfo]
  -> [(T.PostingInfo, T.VisibleNum)]
addVisibleNum ls = zip ls (map T.VisibleNum [0..])

-- * Step 5 - multiply into tranches
tranches ::
  Bounded t
  => [(T.PostingInfo, T.VisibleNum)]
  -> [(T.PostingInfo, T.VisibleNum, t)]
tranches ls =
  (\(p, vn) t -> (p, vn, t))
  <$> ls
  <*> [minBound, maxBound]

-- * Step 6 - multiply to array
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

-- * Step 7 - Space claim

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

-- * Step 8 - GrowToFit

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
--
-- The Grower function is supplied with the maximum claimed width of
-- every column.
type Grower c t =
  A.Array c C.Width
  -- ^ Maximum claimed width of every column
  -> Index c t
  -- ^ Address of this particular cell
  -> (T.PostingInfo, Maybe T.ClaimedWidth)
  -- ^ Information on this posting, and contents of this particular
  -- cell
  -> Maybe R.Cell
  -- ^ Supplied cell, if applicable

growCells ::
  (A.Ix c, A.Ix t)
  => Grower c t
  -> A.Array (Index c t) (T.PostingInfo, Maybe T.ClaimedWidth)
  -> A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
growCells f a = fmapArray g a where
  g _ i (p, w) = (p, f maxW i (p, w))
  maxW = widthArray a

-- | Transforms a two-dimensional array of columns and rows into a
-- one-dimensional array indicating the widest row in each column.
widthArray ::
  (A.Ix c, A.Ix r)
  => A.Array (c, r) (a, Maybe T.ClaimedWidth)
  -> A.Array c C.Width
widthArray =
  fmap widestRow
  . Ta.columns
  . fmap snd

-- | Takes an array of rows. Returns the width of the widest row.
widestRow ::
  (A.Ix r)
  => A.Array r (Maybe T.ClaimedWidth)
  -> C.Width
widestRow a = F.foldl' f (C.Width 0) (Ta.OneDim a) where
  f soFar clm = max width soFar where
    width = case clm of
      Nothing -> C.Width 0
      Just cw -> C.Width . T.unClaimedWidth $ cw


-- * Step 9 - Allocation Inspection

-- | Step 9 - Allocation Inspection. This function examines the grid
-- array and makes some calculations. It does not change the
-- individual values within the array. Instead, this function examines
-- the entire array and determines the width of each allocated column.
-- The result is stored in a map, because not every column is
-- allocated; non-allocated columns are omitted from the map. This
-- list is passed on to the calculation made in Step 10.
type AllocInspector c t =
  A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
  -> M.Map c C.Width

-- * Step 10 - Allocate

-- | Step 10 - Allocate. What to do at this phase:
--
-- * GrowToFit, Padding, Empty, and Overran cells - have already
-- supplied a Just Cell from Step 8. Pass that cell along.
--
-- * Allocated cells. If the field is not selected to be in the
-- report, supply a Just empty cell. If the field is selected, use the
-- width stored in the array created in step 9 to size the cell
-- appropriately. Supply the appropriate Just cell.
--
-- * Overrunning cells - supply Nothing.
type Allocator c t =
  M.Map c C.Width
  -> Index c t
  -> (T.PostingInfo, Maybe R.Cell)
  -> Maybe R.Cell

allocateCells ::
  (A.Ix c, A.Ix t, Ord c)
  => AllocInspector c t
  -> Allocator c t
  -> A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
  -> A.Array (Index c t) (T.PostingInfo, Maybe R.Cell)
allocateCells insp f a = fmapArray g a where
  g _ i (p, w) = (p, f fromStep9 i (p, w))
  fromStep9 = insp a

-- * Step 11 - Finalize

-- | Step 11. Finalize all cells, including overruns. What cells should
-- do at this phase:
--
-- * GrowToFit, Empty but padded, padding, overran, allocated cells -
-- these have already supplied a cell. Pass this cell along.
--
-- * Overrunning cells - If the corresponding field is not showing,
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

-- * Step 12 - make chunks
newtype CellArray c t =
  CellArray { unCellArray :: A.Array (Index c t) R.Cell }

instance (A.Ix c, A.Ix t) => R.HasChunk (CellArray c t) where
  chunk (CellArray a) = R.chunk rows where
    rows = F.foldr R.prependRow R.emptyRows rs
    rs = fmap toRow . Ta.OneDim . Ta.rows $ a
    toRow = F.foldr R.prependCell R.emptyRow

-- * Put it all together

report ::
  (A.Ix c, A.Ix t, Bounded c, Bounded t)
  => Claimer c t
  -> Grower c t
  -> AllocInspector c t
  -> Allocator c t
  -> Finalizer c t
  -> (LT.PostingInfo -> Bool) -- ^ Main predicate
  -> ([T.PostingInfo] -> [T.PostingInfo]) -- ^ Post filter
  -> [LT.PostingInfo]
  -> Maybe C.Chunk
report c g ai a f p pf pbs =
  NE.nonEmpty pbs

  >>= (toArray
       . tranches
       . addVisibleNum
       . filterToVisible p pf
       . numberPostings
       . balances)

  >>= (return
       . R.chunk
       . finalize f
       . allocateCells ai a
       . growCells g
       . spaceClaim c)

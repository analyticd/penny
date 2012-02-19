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

-- Step 7 - Space claim
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

-- Step 8 - Allocate GrowToFit
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

-- Step 9 -- Finalize all cells
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

-- Step 9 - make chunks
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
  -> Finalizer c t
  -> [B.PostingBox]
  -> Maybe C.Chunk
report p c g f pbs =
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
   . growCells g
   . spaceClaim c)
  

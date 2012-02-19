module Penny.Cabin.Postings.Grid where

import Control.Applicative (
  (<$>), (<*>), ZipList(ZipList, getZipList))

import qualified Data.Array as A
import Data.Monoid (mempty, mappend)
import qualified Data.Traversable as Tr

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Boxes as B
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Cabin.Postings.Types as T

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
  -> [(B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum)]
numberPostings ls = reverse reversed where
  withPostingNums =
    getZipList
    $ (\(pb, bal) pn -> (pb, bal, pn))
    <$> ZipList ls
    <*> ZipList (map T.PostingNum [0..])
  reversed =
    getZipList
    $ (\(pb, bal, pn) rpn -> (pb, bal, pn, rpn))
    <$> ZipList (reverse withPostingNums)
    <*> ZipList (map T.RevPostingNum [0..])

-- Step 3 - Get visible postings only
filterToVisible ::
  ((B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum) -> Bool)
  -> [(B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum)]
  -> [(B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum)]
filterToVisible b ps = filter b ps  

-- Step 4 - add visible numbers
addVisibleNum ::
  [(B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum)]
  -> [(B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum,
       T.VisibleNum)]
addVisibleNum ls =
  getZipList
  $ (\(pb, bal, pn, rpn) vn -> (pb, bal, pn, rpn, vn))
  <$> ZipList ls
  <*> ZipList (map T.VisibleNum [0..])

-- Step 5 - multiply into tranches
tranches ::
  Bounded t
  => [(B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum,
       T.VisibleNum)]
  -> [(B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum,
       T.VisibleNum, t)]
tranches ls =
  (\(pb, bal, pn, rpn, vn) t -> (pb, bal, pn, rpn, vn, t))
  <$> ls
  <*> [minBound, maxBound]

-- Step 6 - multiply to array
toArray ::
  (Bounded c, Bounded t, A.Ix c, A.Ix t)
  => [(B.PostingBox, Bal.Balance, T.PostingNum, T.RevPostingNum,
       T.VisibleNum, t)]
  -> Maybe (A.Array (c, (T.VisibleNum, t)) T.PostingInfo)
toArray ls =
  if null ls
  then Nothing
  else let
    (_, _, _, _, maxVn, _) = last ls
    b = ((minBound, (T.VisibleNum 0, minBound)),
         (maxBound, (maxVn, maxBound)))
    pair c (pb, bal, pn, rpn, vn, t) =
      ((c, (vn, t)), T.PostingInfo pb bal pn rpn)
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
  -> (T.PostingInfo, Maybe T.ClaimedWidth)

spaceClaim ::
  (A.Ix c, A.Ix t)
  => Claimer c t
  -> A.Array (Index c t) T.PostingInfo
  -> A.Array (Index c t) (T.PostingInfo, Maybe T.ClaimedWidth)
spaceClaim = fmapArray

-- Step 8 - Allocate GrowToFit

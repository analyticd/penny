{-# LANGUAGE TypeFamilies #-}
-- | Queries on 'Penny.Lincoln.Clatch.Clatch'.

module Penny.Queries.Clatch where

import qualified Penny.Lincoln as L

-- |
-- @
-- 'postingL' :: 'L.Clatch' m -> 'L.PostingL' m
-- @
postingL
  :: (L.Viewer a, L.Viewed a ~ L.Converted t1)
  => L.Filtered (L.RunningBalance (L.Sorted (L.Filtered (t, a))))
  -> t1
postingL
  (L.Filtered (L.Sersetted _ (L.RunningBalance _ (L.Sorted (L.Sersetted _
    (L.Filtered (L.Sersetted _ (_, vw)))))))) = pstg
  where
    L.Converted _ pstg = L.onView vw

-- |
-- @
-- 'convertedAmount' :: 'L.Clatch' l -> 'Maybe' 'L.Amount'
-- @
convertedAmount
  :: (L.Viewer a, L.Viewed a ~ L.Converted t1)
  => L.Filtered (L.RunningBalance (L.Sorted (L.Filtered (t, a))))
  -> Maybe L.Amount
convertedAmount
  (L.Filtered (L.Sersetted _ (L.RunningBalance _ (L.Sorted (L.Sersetted _
    (L.Filtered (L.Sersetted _ (_, vw)))))))) = mayAmt
  where
    L.Converted mayAmt _ = L.onView vw

-- |
-- @
-- 'transactionL' :: 'L.Clatch' l -> 'L.TransactionL' l
-- @
transactionL
  :: L.Filtered (L.RunningBalance (L.Sorted (L.Filtered (a, b))))
  -> a
transactionL
  (L.Filtered (L.Sersetted _ (L.RunningBalance _ (L.Sorted (L.Sersetted _
    (L.Filtered (L.Sersetted _ (txn, _)))))))) = txn

-- | Gets the 'L.Serset' resulting from pre-filtering.
--
-- @
-- 'sersetFiltered' :: 'L.Clatch' l -> 'L.Serset'
-- @
sersetPreFiltered
  :: L.Filtered (L.RunningBalance (L.Sorted (L.Filtered a)))
  -> L.Serset
sersetPreFiltered
  (L.Filtered (L.Sersetted _ (L.RunningBalance _ (L.Sorted (L.Sersetted _
    (L.Filtered (L.Sersetted srst _))))))) = srst

-- | Gets the 'L.Serset' resulting from sorting.
--
-- @
-- 'sersetSorted' :: 'L.Clatch' l -> 'L.Serset'
-- @
sersetSorted
  :: L.Filtered (L.RunningBalance (L.Sorted a))
  -> L.Serset
sersetSorted
  (L.Filtered (L.Sersetted _ (L.RunningBalance _ (L.Sorted
    (L.Sersetted srst _))))) = srst

-- | Gets the running balance.
--
-- @
-- 'runningBalance' :: 'L.Clatch' l -> 'L.Balances'
-- @
runningBalance
  :: L.Filtered (L.RunningBalance a)
  -> L.Balances
runningBalance
  (L.Filtered (L.Sersetted _ (L.RunningBalance bal _))) = bal

-- | Gets the 'L.Serset' resulting from post-filtering.
--
-- @
-- 'sersetPostFiltered' :: 'L.Clatch' l -> 'L.Serset'
-- @
sersetPostFiltered
  :: L.Filtered a
  -> L.Serset
sersetPostFiltered (L.Filtered (L.Sersetted srst _)) = srst

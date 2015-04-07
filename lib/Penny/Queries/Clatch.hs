{-# LANGUAGE TypeFamilies #-}
-- | Queries on 'Penny.Clatch.Clatch'.

module Penny.Queries.Clatch where

import Control.Monad
import Data.Sums
import Penny.Amount
import Penny.Balances
import Penny.Clatch
import Penny.Commodity
import Penny.Ledger
import Penny.Number.Rep
import Penny.Qty
import Penny.SeqUtil
import Penny.Serial
import Penny.Trio

-- |
-- @
-- 'postingL' :: 'Clatch' m -> 'PostingL' m
-- @
postingL
  :: (Viewer a, Viewed a ~ Converted t1)
  => Filtered (RunningBalance (Sorted (Filtered (t, a))))
  -> t1
postingL
  (Filtered (Sersetted _ (RunningBalance _ (Sorted (Sersetted _
    (Filtered (Sersetted _ (_, vw)))))))) = pstg
  where
    Converted _ pstg = onView vw

-- |
-- @
-- 'convertedAmount' :: 'Clatch' l -> 'Maybe' 'Amount'
-- @
convertedAmount
  :: (Viewer a, Viewed a ~ Converted t1)
  => Filtered (RunningBalance (Sorted (Filtered (t, a))))
  -> Maybe Amount
convertedAmount
  (Filtered (Sersetted _ (RunningBalance _ (Sorted (Sersetted _
    (Filtered (Sersetted _ (_, vw)))))))) = mayAmt
  where
    Converted mayAmt _ = onView vw

-- |
-- @
-- 'transactionL' :: 'Clatch' l -> 'TransactionL' l
-- @
transactionL
  :: Filtered (RunningBalance (Sorted (Filtered (a, b))))
  -> a
transactionL
  (Filtered (Sersetted _ (RunningBalance _ (Sorted (Sersetted _
    (Filtered (Sersetted _ (txn, _)))))))) = txn

-- | Gets the 'Serset' resulting from pre-filtering.
--
-- @
-- 'sersetFiltered' :: 'Clatch' l -> 'Serset'
-- @
sersetPreFiltered
  :: Filtered (RunningBalance (Sorted (Filtered a)))
  -> Serset
sersetPreFiltered
  (Filtered (Sersetted _ (RunningBalance _ (Sorted (Sersetted _
    (Filtered (Sersetted srst _))))))) = srst

-- | Gets the 'Serset' resulting from sorting.
--
-- @
-- 'sersetSorted' :: 'Clatch' l -> 'Serset'
-- @
sersetSorted
  :: Filtered (RunningBalance (Sorted a))
  -> Serset
sersetSorted
  (Filtered (Sersetted _ (RunningBalance _ (Sorted
    (Sersetted srst _))))) = srst

-- | Gets the running balance.
--
-- @
-- 'runningBalance' :: 'Clatch' l -> 'Balances'
-- @
runningBalance
  :: Filtered (RunningBalance a)
  -> Balances
runningBalance
  (Filtered (Sersetted _ (RunningBalance bal _))) = bal

-- | Gets the 'Serset' resulting from post-filtering.
--
-- @
-- 'sersetPostFiltered' :: 'Clatch' l -> 'Serset'
-- @
sersetPostFiltered
  :: Filtered a
  -> Serset
sersetPostFiltered (Filtered (Sersetted srst _)) = srst

-- | Gets the 'Qty' from the converted Amount, if there is one.
-- Otherwise, gets the 'QtyRep' from the 'Trio', if there is one.
-- Otherwise, gets the 'Qty'.

bestQty
  :: Ledger l
  => Clatch l
  -> l (S3 RepNonNeutralNoSide QtyRepAnyRadix Qty)
bestQty clch = case convertedAmount clch of
  Just (Amount _ qt) -> return $ S3c qt
  Nothing -> do
    tri <- trio . postingL $ clch
    case tri of
      QC qr _ _ -> return $ S3b qr
      Q qr -> return $ S3b qr
      UC nn _ _ -> return $ S3a nn
      U nn -> return $ S3a nn
      _ -> liftM S3c . qty . postingL $ clch

-- | Gets the 'Commodity' from the converted Amount, if there is one.
-- Otherwise, gets the 'Commodity' from the 'PostingL'.
bestCommodity
  :: Ledger l
  => Clatch l
  -> l Commodity
bestCommodity clch = case convertedAmount clch of
  Just (Amount cy _) -> return cy
  Nothing -> commodity . postingL $ clch

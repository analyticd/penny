{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
-- | A 'Clatch' is a single posting from a collection of postings,
-- after considerable processing.  Here are the processing steps:
--
-- 1. Begin with all the transactions returned by 'vault'.
--
-- 2. Convert the amounts.  This allows reports to show both the
-- original amount and an arbitrary amount resulting from a
-- conversion.
--
-- 3. Create a map, 'Renderings', that describes how amounts are
-- rendered, if there is enough information in the 'Trio' to determine
-- this.  This map allows reports to format their numbers based on a
-- histogram of how they have been formatted before.
--
-- 4. Filter the postings.  This way the user can include in the
-- report only the postings she is interested in.
--
-- 5. Sort the postings.
--
-- 6. Compute a running balance of the sorted postings.
--
-- 7. Filter the sorted postings.  This allows the user to see a
-- report that has a running balance that includes an entire set of
-- postings, but that only shows postings of interest.

module Penny.Clatch
  ( Converted(..)
  , Renderings(..)
  , Filtered(..)
  , Sorted(..)
  , RunningBalance(..)
  , Clatch
  , allClatches
  ) where

import Penny.Amount
import Penny.Ledger
import Penny.SeqUtil
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Map.Strict as M
import Control.Monad
import Penny.Commodity
import Penny.NonEmpty
import Penny.Number.Rep
import Penny.Trio
import Penny.Serial
import Penny.Balances
import Penny.Matcher
import Data.Monoid
import qualified Data.Foldable as F


-- # ConvertedPosting

data Converted a = Converted (Maybe Amount) a
  deriving (Functor, T.Traversable, F.Foldable)

-- | Gets the converted Amount if there is one; otherwise, gets the
-- Amount from the PostingL.
bestAmount :: Ledger l => Converted (PostingL l) -> l Amount
bestAmount (Converted mayAmt pstg) = case mayAmt of
  Just a -> return a
  Nothing -> liftM2 Amount (commodity pstg) (qty pstg)

-- | Converts the 'Amount' in a posting to a new amount; useful for
-- commodity conversions.
convertPosting
  :: Ledger l
  => (Amount -> Maybe Amount)
  -- ^
  -> PostingL l
  -- ^
  -> l (Converted (PostingL l))
  -- ^
convertPosting cnv p = do
  qt <- qty p
  cy <- commodity p
  let amt = Amount cy qt
  return $ Converted (cnv amt) p


-- | Fetches all the postings in a transaction and converts their
-- amounts.
convertTransaction
  :: Ledger l
  => (Amount -> Maybe Amount)
  -- ^
  -> TransactionL l
  -- ^
  -> l (Seq (TransactionL l, View (Converted (PostingL l))))
convertTransaction cv txn = do
  pstgs <- postings txn
  converted <- T.mapM (convertPosting cv) pstgs
  return . fmap (txn,) . allViews $ converted

-- | Fetches all transactions in the vault and converts all their
-- amounts.
allConvertedTransactions
  :: Ledger l
  => (Amount -> Maybe Amount)
  -- ^
  -> l (Seq (TransactionL l, View (Converted (PostingL l))))
  -- ^
allConvertedTransactions cv = do
  itms <- liftM join vault
  liftM join . T.mapM (convertTransaction cv) . rights $ itms

-- | Map describing how different 'Commodity' are rendered.
newtype Renderings = Renderings
  (M.Map Commodity
         (NonEmpty (Arrangement, Either (Seq RadCom) (Seq RadPer))))
  deriving (Eq, Ord, Show)

-- | Given a ConvertedPostingView, update the Renderings map.
updateRenderings
  :: Ledger l
  => Renderings
  -- ^
  -> (a, View (Converted (PostingL l)))
  -- ^
  -> l Renderings
  -- ^
updateRenderings (Renderings mp) (_, pv) = liftM f (trio pstg)
  where
    Converted _ pstg = onView pv
    f tri = case trioRendering tri of
      Nothing -> Renderings mp
      Just (cy, ar, ei) -> Renderings $ case M.lookup cy mp of
        Nothing -> M.insert cy (NonEmpty (ar, ei) S.empty) mp
        Just (NonEmpty o1 os) -> M.insert cy
          (NonEmpty o1 (os |> (ar, ei))) mp

-- | Things that have been filtered.  So for instance after filtering
-- something of type @[Int]@, the result would be @[Filtered Int]@.
-- The 'Filtered' type contains a 'Sersetted', along with the original
-- thing that was filtered.  The 'Sersetted' should contain the
-- serials /after/ filtering.
newtype Filtered a = Filtered (Sersetted a)
  deriving (Eq, Ord, Show, Functor, F.Foldable, T.Traversable)

-- | Filter a 'Seq' of items using a 'Matcher'; delivers the 'Matcher'
-- log.
filterWithSerials
  :: Monad m
  => Matcher t m a
  -- ^
  -> Seq t
  -- ^
  -> m (Seq (Seq Message), Seq (Filtered t))
  -- ^
filterWithSerials mr = liftM (fmap addSeqs) . filterSeq mr
  where
    addSeqs = fmap (\(a, srst) -> Filtered (Sersetted srst a))
              . serialNumbers


-- | Things that have been sorted.  So for instance after sorting
-- something of type @[Int]@, the result would be @[Sorted Int]@.  The
-- 'Sorted' type contains a 'Sersetted', along with the original thing
-- that was filtered; the 'Sersetted' should contain the serials
-- /after/ sorting.
newtype Sorted a = Sorted (Sersetted a)
  deriving (Eq, Ord, Show, Functor, F.Foldable, T.Traversable)

-- | Sorts a 'Seq', with effects.
sortConverted
  :: (F.Foldable c, Monad m)
  => c (Seq a -> m (Seq a))
  -- ^ Sorting functions.
  -> Seq a
  -- ^
  -> m (Seq (Sorted a))
  -- ^
sortConverted sorters = liftM (fmap Sorted) . liftM assignSersetted . sorter
  where
    sorter = F.foldl (>=>) return sorters

-- | Thing that is accompanied by a running balance.
data RunningBalance a = RunningBalance Balances a
  deriving (Functor, F.Foldable, T.Traversable)

-- | Compute running balances and add them as a 'RunningBalance'.
addRunningBalances
  :: (Ledger m, T.Traversable c)
  => c (Sorted (Filtered (a, View (Converted (PostingL m)))))
  -- ^
  -> m (c (RunningBalance (Sorted (Filtered (a, View (Converted (PostingL m)))))))
  -- ^
addRunningBalances
  = liftM (snd . T.mapAccumL addBal mempty)
  . T.mapM addBestAmount
  where
    addBestAmount (srtd@(Sorted (Sersetted _ (Filtered (Sersetted _ (_, vw))))))
      = liftM2 (,) (return srtd) (bestAmount . onView $ vw)
    addBal acc (fl, amt) = (acc', new)
      where
        acc' = acc <> c'Balances'Amount amt
        new = RunningBalance acc' fl


-- | A 'Clatch' summarizes a posting after it has been converted,
-- placed in a 'View', pre-filtered, sorted, had a running balance
-- added, and post-filtered.
type Clatch m =
  Filtered (RunningBalance
    (Sorted (Filtered (TransactionL m, View (Converted (PostingL m))))))

-- | Compute 'Clatch' for every posting in a vault.
allClatches
  :: (Ledger m, F.Foldable c)
  => (Amount -> Maybe Amount)
  -- ^ Converts Amounts
  -> Matcher (TransactionL m, View (Converted (PostingL m))) m a
  -- ^ Filters postings after they have been converted; this is known
  -- as the \"pre-filter\".
  -> c (Seq (Filtered (TransactionL m, View (Converted (PostingL m))))
            -> m (Seq (Filtered (TransactionL m, View (Converted (PostingL m))))))
  -- ^ List of functions to sort postings after they have been filtered
  -> Matcher (RunningBalance
      (Sorted (Filtered (TransactionL m, View (Converted (PostingL m)))))) m b
  -- ^ Filters postings after they have been sorted; this is known as
  -- the \"post-filter\".
  -> m ( ( Seq (Seq Message)
         , Renderings
         , Seq (Seq Message)
         )
       , Seq (Clatch m)
       )
  -- ^ Returns messages from the pre-filter, 'Renderings', messages
  -- from the post-filter, and the clatches themselves.
allClatches conv mtcrConverted srtr mtcrSorted = do
  txns <- allConvertedTransactions conv
  rndgs <- F.foldlM updateRenderings (Renderings M.empty) txns
  (msgsFiltConverted, filt) <- filterWithSerials mtcrConverted txns
  srtd <- sortConverted srtr filt
  withBals <- addRunningBalances srtd
  (msgsFiltSrtd, filt') <- filterWithSerials mtcrSorted withBals
  return ((msgsFiltConverted, rndgs, msgsFiltSrtd), filt')

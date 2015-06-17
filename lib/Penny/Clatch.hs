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

module Penny.Clatch where

import Control.Lens
import Control.Monad
import Penny.Converted
import Penny.Ledger (TreeL, Ledger)
import qualified Penny.Ledger
import Penny.SeqUtil
import Data.Foldable (Foldable)
import Penny.Transbox
import Penny.Prefilt
import Penny.Viewpost
import Penny.Sorted
import Penny.Filtered
import Data.Sequence (Seq)
import Data.Monoid
import Penny.Balance
import Data.Sums

type Clatch l
  = Transbox l (Viewpost l (Converted (Filtered (Sorted
      (RunningBalance (Filtered ()))))))

pstgMeta :: Ledger l => Transbox l (Viewpost l a) -> l (Seq (TreeL l))
pstgMeta = Penny.Ledger.pstgMeta . view (transboxee.viewpost.onView)

allMeta :: Ledger l => Transbox l (Viewpost l a) -> l (Seq (TreeL l))
allMeta t = liftM2 mappend (pstgMeta t) (txnMeta t)


createViewposts :: Ledger l => Transbox l a -> l (Seq (Transbox l (Viewpost l ())))
createViewposts tbox
  = return . fmap (\vw -> Transbox (_transaction tbox) (Viewpost vw ()))
           . allViews
  <=< Penny.Ledger.postings . _transaction
  $ tbox

createConverted
  :: (Ledger l, Traversable t)
  => Converter
  -> t (Viewpost l a)
  -> l (t (Viewpost l (Converted ())))
createConverted converter = traverse f
  where
    f (Viewpost vw _) = do
      converted <- convertPosting converter (_onView vw)
      let converted' = () <$ converted
      return $ Viewpost vw converted'

createFiltered
  :: Ledger l
  => (Transbox l (Viewpost l (Converted ())) -> l Bool)
  -- ^ Predicate
  -> Seq (Transbox l (Viewpost l (Converted ())))
  -> Seq (Transbox l (Viewpost l (Converted (Filtered ()))))
createFiltered = undefined
{-

module Penny.Clatch
  ( Converted(..)
  , Filtered(..)
  , Sorted(..)
  , RunningBalance(..)
  , Clatch
  , allClatches

  -- * Queries
  , postingL
  , convertedAmount
  , transactionL
  , sersetPreFiltered
  , sersetSorted
  , sersetPostFiltered
  , originalQtyRep
  , bestQty
  , bestQtyRep
  , bestCommodity
  , runningBalance
  ) where

import Penny.Amount
import Penny.Ledger
import Penny.SeqUtil
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Map.Strict as M
import Control.Monad
import Penny.NonEmpty
import Penny.Trio
import Penny.Serial
import Penny.Balance
import Penny.Matcher
import Penny.Popularity hiding (rights)
import Data.Monoid
import qualified Data.Foldable as F

import Data.Sums
import Penny.Commodity
import Penny.Representation
import Penny.Qty


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
    Converted _ pstg = _onView pv
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
data RunningBalance a = RunningBalance Balance a
  deriving (Functor, F.Foldable, T.Traversable)

-- | Compute running balances and add them as a 'RunningBalance'.
addRunningBalance
  :: (Ledger m, T.Traversable c)
  => c (Sorted (Filtered (a, View (Converted (PostingL m)))))
  -- ^
  -> m (c (RunningBalance (Sorted (Filtered (a, View (Converted (PostingL m)))))))
  -- ^
addRunningBalance
  = liftM (snd . T.mapAccumL addBal mempty)
  . T.mapM addBestAmount
  where
    addBestAmount (srtd@(Sorted (Sersetted _ (Filtered (Sersetted _ (_, vw))))))
      = liftM2 (,) (return srtd) (bestAmount . _onView $ vw)
    addBal acc (fl, amt) = (acc', new)
      where
        acc' = acc <> c'Balance'Amount amt
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
  withBals <- addRunningBalance srtd
  (msgsFiltSrtd, filt') <- filterWithSerials mtcrSorted withBals
  return ((msgsFiltConverted, rndgs, msgsFiltSrtd), filt')

--
-- # Queries
--

-- | Gets the 'PostingL' from the 'Clatch'.
postingL :: Clatch m -> PostingL m
postingL
  (Filtered (Sersetted _ (RunningBalance _ (Sorted (Sersetted _
    (Filtered (Sersetted _ (_, vw)))))))) = pstg
  where
    Converted _ pstg = _onView vw

-- | Gets the 'Amount' after conversion, if any conversion took place.
convertedAmount
  :: Clatch l
  -> Maybe Amount
convertedAmount
  (Filtered (Sersetted _ (RunningBalance _ (Sorted (Sersetted _
    (Filtered (Sersetted _ (_, vw)))))))) = mayAmt
  where
    Converted mayAmt _ = _onView vw

-- | Gets the 'TransactionL' from a 'Clatch'.
transactionL
  :: Clatch l
  -> TransactionL l
transactionL
  (Filtered (Sersetted _ (RunningBalance _ (Sorted (Sersetted _
    (Filtered (Sersetted _ (txn, _)))))))) = txn

-- | Gets the 'Serset' resulting from pre-filtering.
sersetPreFiltered :: Clatch l -> Serset
sersetPreFiltered
  (Filtered (Sersetted _ (RunningBalance _ (Sorted (Sersetted _
    (Filtered (Sersetted srst _))))))) = srst

-- | Gets the 'Serset' resulting from sorting.
sersetSorted :: Clatch l -> Serset
sersetSorted
  (Filtered (Sersetted _ (RunningBalance _ (Sorted
    (Sersetted srst _))))) = srst

-- | Gets the running balance.
runningBalance :: Clatch l -> Balance
runningBalance
  (Filtered (Sersetted _ (RunningBalance bal _))) = bal

-- | Gets the 'Serset' resulting from post-filtering.
--
-- @
-- 'sersetPostFiltered' :: 'Clatch' l -> 'Serset'
-- @
sersetPostFiltered :: Clatch l -> Serset
sersetPostFiltered (Filtered (Sersetted srst _)) = srst

-- | Gets the 'Qty' using the 'Trio' in the 'PostingL'.
originalQtyRep
  :: Ledger l
  => Clatch l
  -- ^
  -> l (S3 RepNonNeutralNoSide QtyRepAnyRadix Qty)
originalQtyRep clch = (trio . postingL $ clch) >>= conv
  where
    conv tri = case tri of
      QC qr _ _ -> return $ S3b qr
      Q qr -> return $ S3b qr
      UC nn _ _ -> return $ S3a nn
      U nn -> return $ S3a nn
      _ -> liftM S3c . qty . postingL $ clch

-- | Gets the 'Qty' from the converted 'Amount', if there is one;
-- otherwise, get the original 'Qty'.
bestQty :: Ledger l => Clatch l -> l Qty
bestQty clch = case convertedAmount clch of
  Just (Amount _ qt) -> return qt
  Nothing -> qty . postingL $ clch

-- | Gets the 'Qty' from the converted Amount, if there is one.
-- Otherwise, gets the 'QtyRep' from the 'Trio', if there is one.
-- Otherwise, gets the 'Qty'.

bestQtyRep
  :: Ledger l
  => Clatch l
  -> l (S3 RepNonNeutralNoSide QtyRepAnyRadix Qty)
bestQtyRep clch = case convertedAmount clch of
  Just (Amount _ qt) -> return $ S3c qt
  Nothing -> originalQtyRep clch

-- | Gets the 'Commodity' from the converted Amount, if there is one.
-- Otherwise, gets the 'Commodity' from the 'PostingL'.
bestCommodity
  :: Ledger l
  => Clatch l
  -> l Commodity
bestCommodity clch = case convertedAmount clch of
  Just (Amount cy _) -> return cy
  Nothing -> commodity . postingL $ clch
-}

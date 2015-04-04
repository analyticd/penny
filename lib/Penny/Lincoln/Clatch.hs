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
-- 6. (TODO) compute a running balance of the sorted postings.
--
-- 7. Filter the sorted postings.  This allows the user to see a
-- report that has a running balance that includes an entire set of
-- postings, but that only shows postings of interest.

module Penny.Lincoln.Clatch where

import Control.Applicative
import Penny.Lincoln.Amount
import Penny.Lincoln.Ledger
import Penny.Lincoln.SeqUtil
import Data.Sequence
  (Seq, viewl, ViewL(..), (|>), viewr, ViewR(..), (<|))
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Map.Strict as M
import Control.Monad
import Penny.Lincoln.Commodity
import Penny.Lincoln.NonEmpty
import Penny.Lincoln.Rep
import Penny.Lincoln.Trio
import Penny.Lincoln.Serial
import Penny.Lincoln.Balances
import Penny.Lincoln.Matcher
import Data.Monoid
import Data.Bifunctor
import qualified Data.Foldable as F


-- # ConvertedPosting

newtype Converted a = Converted a
  deriving (Functor, T.Traversable, F.Foldable)

-- | A posting, along with its 'Amount' and (possibly) its 'Converted'
-- 'Amount'.
data ConvertedPosting l
  = ConvertedPosting (PostingL l) Amount (Maybe (Converted Amount))

convertPosting
  :: Ledger l
  => (Amount -> Maybe (Converted Amount))
  -> PostingL l
  -> l (ConvertedPosting l)
convertPosting cnv p = do
  qty <- quant p
  cy <- curren p
  let amt = Amount cy qty
  return $ ConvertedPosting p amt (cnv amt)


convertTransaction
  :: Ledger l
  => (Amount -> Maybe (Converted Amount))
  -- ^
  -> TransactionL l
  -- ^
  -> l (Seq (TransactionL l, View (ConvertedPosting l)))
convertTransaction cv txn = do
  pstgs <- plinks txn
  converted <- T.mapM (convertPosting cv) pstgs
  return . fmap (txn,) . allViews $ converted

allConvertedTransactions
  :: Ledger l
  => (Amount -> Maybe (Converted Amount))
  -> l (Seq (TransactionL l, View (ConvertedPosting l)))
allConvertedTransactions cv = do
  itms <- liftM join vault
  liftM join . T.mapM (convertTransaction cv) . rights $ itms

-- | Map describing how different 'Commodity' are rendered.
newtype Renderings = Renderings
  (M.Map Commodity
         (NonEmpty (Arrangement, Either (Seq RadCom) (Seq RadPer))))
  deriving (Eq, Ord, Show)

-- | Given a PostingView, selects the best Amount from the
-- original Amount and the 'Converted' one.  If there is a 'Converted'
-- amount, use that; otherwise, use the original 'Amount'.
--
-- Fails if the cursor is not on an item.
clatchAmount :: (a, View (ConvertedPosting l)) -> Maybe Amount
clatchAmount (_, pv) = fmap get (viewCurrentItem pv)
  where
    get (ConvertedPosting _ am mayCv) = case mayCv of
      Nothing -> am
      Just (Converted am') -> am'

-- | Given a ConvertedPostingView, update the Renderings map.
updateRenderings
  :: Ledger l
  => Renderings
  -> (a, View (ConvertedPosting l))
  -> l Renderings
updateRenderings (Renderings mp) (_, pv)
  = case viewCurrentItem pv of
  Nothing -> return $ Renderings mp
  Just (ConvertedPosting pstg _ _) -> liftM f (triplet pstg)
  where
    f tri = case trioRendering tri of
      Nothing -> Renderings mp
      Just (cy, ar, ei) -> Renderings $ case M.lookup cy mp of
        Nothing -> M.insert cy (NonEmpty (ar, ei) S.empty) mp
        Just (NonEmpty o1 os) -> M.insert cy
          (NonEmpty o1 (os |> (ar, ei))) mp

newtype Filtered a = Filtered (Sersetted a)
  deriving (Eq, Ord, Show, Functor, F.Foldable, T.Traversable)

filterWithSerials
  :: Monad m
  => Matcher t m a
  -> Seq t
  -> m (Seq (Maybe (Seq Message)), Seq (Filtered t))
filterWithSerials mr = liftM (fmap addSeqs) . filterSeq mr
  where
    addSeqs = fmap (\(a, srst) -> Filtered (Sersetted srst a))
              . serialNumbers

filterConvertedPostings
  :: Ledger l
  => Matcher (TransactionL l, View (ConvertedPosting l)) l a
  -> Seq (TransactionL l, View (ConvertedPosting l))
  -> l ( Seq (Maybe (Seq Message))
       , Seq (Filtered (TransactionL l, View (ConvertedPosting l)))
       )
filterConvertedPostings = filterWithSerials

newtype Sorted a = Sorted (Sersetted a)
  deriving (Eq, Ord, Show, Functor, F.Foldable, T.Traversable)

sortConverted
  :: (F.Foldable c, Monad m)
  => c (Seq a -> m (Seq a))
  -> Seq a
  -> m (Seq (Sorted a))
sortConverted sorters = liftM (fmap Sorted) . liftM assignSersetted . sorter
  where
    sorter = F.foldl (>=>) return sorters

type Clatch m =
  Filtered (Sorted (Filtered (TransactionL m, View (ConvertedPosting m))))

allClatches
  :: (Ledger m, F.Foldable c)
  => (Amount -> Maybe (Converted Amount))
  -- ^ Converts Amounts
  -> Matcher (TransactionL m, View (ConvertedPosting m)) m a
  -- ^ Filters postings after they have been converted
  -> c (Seq (Filtered (TransactionL m, View (ConvertedPosting m)))
            -> m (Seq (Filtered (TransactionL m, View (ConvertedPosting m)))))
  -- ^ List of functions to sort postings after they have been filtered
  -> Matcher (Sorted (Filtered (TransactionL m, View (ConvertedPosting m)))) m b
  -- ^ Filters postings after they have been sorted
  -> m ( ( Seq (Maybe (Seq Message))
         , Renderings
         , Seq (Maybe (Seq Message))
         )
       , Seq (Clatch m)
       )
allClatches conv mtcrConverted srtr mtcrSorted = do
  txns <- allConvertedTransactions conv
  rndgs <- F.foldlM updateRenderings (Renderings M.empty) txns
  (msgsFiltConverted, filt) <- filterWithSerials mtcrConverted txns
  srtd <- sortConverted srtr filt
  (msgsFiltSrtd, filt') <- filterWithSerials mtcrSorted srtd
  return ((msgsFiltConverted, rndgs, msgsFiltSrtd), filt')

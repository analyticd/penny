{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
-- | ConvertedPostingViewes and associated types.  These types help
-- summarize a collection of postings.
--
-- First, a set of transactions is processed to create a set of
-- 'ConvertedPostingView'.  Each transaction can give rise to multiple
-- 'ConvertedPostingView'; there is one 'ConvertedPostingView' per
-- posting.  The 'ConvertedPostingView' are not assigned serials; if
-- you're interested in the order of the 'ConvertedPostingView',
-- examine the 'GlobalSer' of the respective 'PostingL'.
--
-- Then, the set of 'ConvertedPostingView' is filtered.  This yields a
-- set of 'FilteredConvertedPostingView'.  Each
-- 'FilteredConvertedPostingView' is assigned a 'Serset'.
--
-- Next, the set of 'ConvertedPostingView' are sorted.  This yields a
-- 'SortedFilteredConvertedPostingView'.  The
-- 'SortedFilteredConvertedPostingView' is accompanied by its own
-- serial.  Each 'SortedFilteredConvertedPostingView' is also assigned
-- a 'Balances' corresponding to where it is relative to the other
-- 'SortedFilteredConvertedPostingView'.
--
-- Next, the 'SortedFilteredConvertedPostingView' are filtered.  This
-- yields a set of 'Tranche'.  Each 'Tranche' is assigned a serial.
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
  => (a, View (ConvertedPosting l))
  -> Renderings
  -> l Renderings
updateRenderings (_, pv) (Renderings mp)
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

data Filtered a = Filtered Serset a
  deriving (Eq, Ord, Show, Functor, F.Foldable, T.Traversable)

filterWithSerials
  :: Monad m
  => Matcher t m a
  -> Seq t
  -> m (Seq (Maybe (Seq Message)), Seq (Filtered t))
filterWithSerials mr = liftM (fmap addSeqs) . filterSeq mr
  where
    addSeqs = fmap (\(a, srst) -> Filtered srst a)
              . serialNumbers

filterConvertedPostings
  :: Ledger l
  => Matcher (TransactionL l, View (ConvertedPosting l)) l a
  -> Seq (TransactionL l, View (ConvertedPosting l))
  -> l ( Seq (Maybe (Seq Message))
       , Seq (Filtered (TransactionL l, View (ConvertedPosting l)))
       )
filterConvertedPostings mr sq = do
  (filtered, msgs) <- filterSeq mr sq
  undefined
{-

-- | A 'Seq' of 'FilteredConvertedPostingView' results from the
-- filtering of a 'Seq' of 'ConvertedPostingView'.  Each
-- 'FilteredConvertedPostingView' is accompanied by a 'Serset'.
newtype FilteredConvertedPostingView l
  = FilteredConvertedPostingView (Sersetted (ConvertedPostingView l))

-- | Computes a series of 'FilteredConvertedPostingView' from a series
-- of 'ConvertedPostingView' by filtering using a given predicate.
slices
  :: Ledger l
  => Matcher (ConvertedPostingView l) l a
  -> Seq (ConvertedPostingView l)
  -> l ( Seq (FilteredConvertedPostingView l)
       , Seq (Maybe (Seq Message)))
slices pd cltchs = do
  (withSers, rslt) <- liftM (first serialNumbers)
    . filterSeq pd $ cltchs
  let mkView = FilteredConvertedPostingView . uncurry Sersetted
  return (fmap mkView withSers, rslt)


-- | A 'Seq' of 'SortedFilteredConvertedPostingView' is the result of
-- the sorting of a 'Seq' of 'FilteredConvertedPostingView'.  Each
-- 'SortedFilteredConvertedPostingView' has an accompanying 'Serset'.
-- Also, this is the point at which 'Balances' are computed.  The
-- 'Balances' is a running balance, computed using the 'Amount'
-- returned by 'clatchAmount'.
newtype SortedFilteredConvertedPostingView l
  = SortedFilteredConvertedPostingView
    (Sersetted (FilteredConvertedPostingView l, Balances))

splints
  :: Monad m
  => ( Seq (FilteredConvertedPostingView m)
       -> m (Seq (FilteredConvertedPostingView m)))
  -- ^ Sorter
  -> Seq (FilteredConvertedPostingView m)
  -> m (Seq (SortedFilteredConvertedPostingView m))
splints srtr sq
  = liftM mkSortedFilteredConvertedPostingViews $ srtr sq
  where
    mkSortedFilteredConvertedPostingViews = addBals . serialNumbers
    addBals = snd . T.mapAccumL addBal mempty
    addBal bal
      (slce@(FilteredConvertedPostingView (Sersetted clch _)), srst)
      = (bal', splt)
      where
        bal' = addAmountToBalances (clatchAmount clch) bal
        splt = SortedFilteredConvertedPostingView
          (Sersetted (slce, bal') srst)

newtype Filtered a = Filtered (Sersetted a)
  deriving (Functor, T.Traversable, F.Foldable)

-- | A 'Seq' of 'Tranche' is the result of the filtering of a 'Seq' of
-- 'SortedFilteredConvertedPostingView'.  Each 'Tranche' comes with a
-- 'Serset'.
newtype Tranche l
  = Tranche (Sersetted (SortedFilteredConvertedPostingView l))

tranches
  :: Monad m
  => Matcher (SortedFilteredConvertedPostingView m) m a
  -> Seq (SortedFilteredConvertedPostingView m)
  -> m (Seq (Tranche m), Seq (Maybe (Seq Message)))
tranches pd
  = liftM (first (fmap (Tranche . uncurry Sersetted)))
  . liftM (first serialNumbers)
  . filterSeq pd


-- | Pulls together many functions in this module to deliver a quad
-- @(w, x, y, z)@, where @w@ is a list of all Tranche, @x@ is a list
-- of the descriptions from filtering the 'ConvertedPostingView', @y@
-- is the 'Renderings', and @z@ is a list of the descriptions from
-- filtering the 'SortedFilteredConvertedPostingView'.  The
-- 'ConvertedPostingView' are pulled ultimately by using 'vault'.

allTranches
  :: Ledger l
  => (Amount -> Maybe Converted)
  -- ^ Converts the original 'Amount' to a different one.
  -> Matcher (ConvertedPostingView l) l a
  -- ^ Filters 'ConvertedPostingView'
  -> (Seq (FilteredConvertedPostingView l)
          -> l (Seq (FilteredConvertedPostingView l)))
  -- ^ Sorts 'FilteredConvertedPostingView'
  -> Matcher (SortedFilteredConvertedPostingView l) l b
  -- ^ Filters 'SortedFilteredConvertedPostingView'
  -> l ( Seq (Tranche l)
       , Seq (Maybe (Seq Message))
       , Renderings
       , Seq (Maybe (Seq Message))
       )
allTranches conv pdCltch srtr pd = do
  cltchs <- allConvertedPostingViewes conv
  rndgs <- F.foldrM updateRenderings (Renderings M.empty) cltchs
  (slcs, rsltSlcs) <- slices pdCltch cltchs
  splnts <- splints srtr slcs
  (trchs, rsltTrchs) <- tranches pd splnts
  return (trchs, rsltSlcs, rndgs, rsltTrchs)
-}

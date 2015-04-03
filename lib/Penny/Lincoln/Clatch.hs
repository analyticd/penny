{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
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

-- | An 'Amount' that has been converted from the native 'Amount' in
-- the posting.
newtype Converted = Converted Amount

-- | A posting, along with its 'Amount' and (possibly) its 'Converted'
-- 'Amount'.
data ConvertedPosting l
  = ConvertedPosting (PostingL l) Amount (Maybe Converted)

-- # ConvertedPostingViewes

-- | A view on postings; gives you a posting, its sibling postings,
-- and the parent transaction.
data ConvertedPostingView l = ConvertedPostingView
  (TransactionL l) (Seq (ConvertedPosting l))
  (ConvertedPosting l) (Seq (ConvertedPosting l))
  -- ^ @ConvertedPostingView t l c r@, where
  --
  -- @t@ is the transaction giving rise to this
  -- 'ConvertedPostingView',
  --
  -- @l@ are postings on the left.  Closer siblings are at the
  -- right end of the list.
  --
  -- @c@ is the current posting.
  --
  -- @r@ are postings on the right.  Closer siblings are at
  -- the left end of the list.

nextConvertedPostingView
  :: ConvertedPostingView l
  -> Maybe (ConvertedPostingView l)
nextConvertedPostingView (ConvertedPostingView t l c r)
  = case viewl r of
      EmptyL -> Nothing
      x :< xs -> Just $ ConvertedPostingView t (l |> c) x xs

prevConvertedPostingView
  :: ConvertedPostingView l
  -> Maybe (ConvertedPostingView l)
prevConvertedPostingView (ConvertedPostingView t l c r)
  = case viewr l of
      EmptyR -> Nothing
      xs :> x -> Just $ ConvertedPostingView t xs x (c <| r)

-- | Get all the 'ConvertedPosting' from a 'PostingL'; also
-- performs any requested conversions.
bevies
  :: Ledger l
  => (Amount -> Maybe Converted)
  -- ^ How to convert 'Amount's
  -> Seq (PostingL l)
  -> l (Seq (ConvertedPosting l))
bevies conv = T.mapM mkConvertedPosting
  where
    mkConvertedPosting pstg = liftM2 f (quant pstg) (curren pstg)
      where
        f q c = ConvertedPosting pstg (Amount c q) (conv (Amount c q))

-- | Creates all 'ConvertedPostingView' from a list of
-- 'ConvertedPosting'.
beviesToConvertedPostingViewes
  :: TransactionL l
  -- ^ Parent transaction to the collection of 'ConvertedPosting'
  -> Seq (ConvertedPosting l)
  -> Seq (ConvertedPostingView l)
beviesToConvertedPostingViewes txn = go S.empty
  where
    go onLeft onRight = case viewl onRight of
      EmptyL -> S.empty
      x :< xs -> ConvertedPostingView txn onLeft x xs
                                      <| go (onLeft |> x) xs

-- | Creates all 'ConvertedPostingView' from a single 'TransactionL'.
clatches
  :: Ledger l
  => (Amount -> Maybe Converted)
  -- ^ Performs any conversions.
  -> TransactionL l
  -- ^ Transaction containing the postings to create
  -- 'ConvertedPostingView' from.
  -> l (Seq (ConvertedPostingView l))
clatches conv txn = do
  pstgs <- plinks txn
  bvys <- bevies conv pstgs
  return $ beviesToConvertedPostingViewes txn bvys

-- | Create all 'ConvertedPostingView' from a given 'Ledger'.  The
-- transactions are retrieved using 'ledgerItems'.
allConvertedPostingViewes
  :: Ledger l
  => (Amount -> Maybe Converted)
  -> l (Seq (ConvertedPostingView l))
allConvertedPostingViewes conv = do
  itms <- liftM join vault
  let txns = rights itms
  liftM join $ T.mapM (clatches conv) txns

-- | Map describing how different 'Commodity' are rendered.
newtype Renderings = Renderings
  (M.Map Commodity
         (NonEmpty (Arrangement, Either (Seq RadCom) (Seq RadPer))))
  deriving (Eq, Ord, Show)

-- | Given a ConvertedPostingView, selects the best Amount from the
-- original Amount and the 'Converted' one.  If there is a 'Converted'
-- amount, use that; otherwise, use the original 'Amount'.
clatchAmount :: ConvertedPostingView l -> Amount
clatchAmount (ConvertedPostingView _ _
              (ConvertedPosting _ amt mayConv) _)
  = maybe amt (\(Converted a) -> a) mayConv

-- | Given a ConvertedPostingView, update the Renderings map.
updateRenderings
  :: Ledger l
  => ConvertedPostingView l
  -> Renderings
  -> l Renderings
updateRenderings (ConvertedPostingView _ _
                  (ConvertedPosting pstg _ _) _)
  (Renderings mp) = liftM f (triplet pstg)
  where
    f tri = case trioRendering tri of
      Nothing -> Renderings mp
      Just (cy, ar, ei) -> Renderings $ case M.lookup cy mp of
        Nothing -> M.insert cy (NonEmpty (ar, ei) S.empty) mp
        Just (NonEmpty o1 os) -> M.insert cy
          (NonEmpty o1 (os |> (ar, ei))) mp

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

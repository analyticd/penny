-- | Clatches and associated types.  These types help summarize a
-- collection of postings.
--
-- First, a set of transactions is processed to create a set of
-- 'Clatch'.  Each transaction can give rise to multiple 'Clatch';
-- there is one 'Clatch' per posting.  The 'Clatch' are not assigned
-- serials; if you're interested in the order of the 'Clatch', examine
-- the 'GlobalSer' of the respective 'PostingL'.
--
-- Then, the set of 'Clatch' is filtered.  This yields a set of
-- 'Slice'.  Each 'Slice' is assigned a 'Serset'.
--
-- Next, the set of 'Clatch' are sorted.  This yields a 'Splint'.  The
-- 'Splint' is accompanied by its own serial.  Each 'Splint' is also
-- assigned a 'Balances' corresponding to where it is relative to the
-- other 'Splint'.
--
-- Next, the 'Splint' are filtered.  This yields a set of 'Tranche'.
-- Each 'Tranche' is assigned a serial.
module Penny.Lincoln.Clatch where

import Prednote hiding (maybe)
import Penny.Lincoln.Amount
import Penny.Lincoln.Ledger
import Penny.Lincoln.Filter
import Penny.Lincoln.SeqUtil
import Data.Sequence
  (Seq, viewl, ViewL(..), (|>), viewr, ViewR(..), (<|))
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad
import Penny.Lincoln.Commodity
import Penny.Lincoln.NonEmpty
import Penny.Lincoln.Rep
import Penny.Lincoln.Trio
import Penny.Lincoln.Serial
import Penny.Lincoln.Balances
import Data.Monoid
import Data.Bifunctor
import qualified Data.Foldable as F

-- # Bevy

-- | An 'Amount' that has been converted from the native 'Amount' in
-- the posting.
newtype Converted = Converted Amount

-- | A posting, along with its 'Amount' and (possibly) its 'Converted'
-- 'Amount'.
data Bevy l = Bevy (PostingL l) Amount (Maybe Converted)

-- # Clatches

-- | A view on postings; gives you a posting, its sibling postings,
-- and the parent transaction.
data Clatch l
  = Clatch (TransactionL l) (Seq (Bevy l))
           (Bevy l) (Seq (Bevy l))
  -- ^ @Clatch t l c r@, where
  --
  -- @t@ is the transaction giving rise to this 'Clatch',
  --
  -- @l@ are postings on the left.  Closer siblings are at the
  -- right end of the list.
  --
  -- @c@ is the current posting.
  --
  -- @r@ are postings on the right.  Closer siblings are at
  -- the left end of the list.

nextClatch :: Clatch l -> Maybe (Clatch l)
nextClatch (Clatch t l c r) = case viewl r of
  EmptyL -> Nothing
  x :< xs -> Just $ Clatch t (l |> c) x xs

prevClatch :: Clatch l -> Maybe (Clatch l)
prevClatch (Clatch t l c r) = case viewr l of
  EmptyR -> Nothing
  xs :> x -> Just $ Clatch t xs x (c <| r)

-- | Get all the 'Bevy' from a 'PostingL'; also performs any requested
-- conversions.
bevies
  :: Ledger l
  => (Amount -> Maybe Converted)
  -- ^ How to convert 'Amount's
  -> Seq (PostingL l)
  -> l (Seq (Bevy l))
bevies conv = T.traverse mkBevy
  where
    mkBevy pstg = f <$> postingQty pstg <*> postingCommodity pstg
      where
        f q c = Bevy pstg (Amount c q) (conv (Amount c q))

-- | Creates all 'Clatch' from a list of 'Bevy'.
beviesToClatches
  :: TransactionL l
  -- ^ Parent transaction to the collection of 'Bevy'
  -> Seq (Bevy l)
  -> Seq (Clatch l)
beviesToClatches txn = go S.empty
  where
    go onLeft onRight = case viewl onRight of
      EmptyL -> S.empty
      x :< xs -> Clatch txn onLeft x xs <| go (onLeft |> x) xs

-- | Creates all 'Clatch' from a single 'TransactionL'.
clatches
  :: Ledger l
  => (Amount -> Maybe Converted)
  -- ^ Performs any conversions.
  -> TransactionL l
  -- ^ Transaction containing the postings to create 'Clatch' from.
  -> l (Seq (Clatch l))
clatches conv txn = do
  pstgs <- postings txn
  bvys <- bevies conv pstgs
  return $ beviesToClatches txn bvys

-- | Create all 'Clatch' from a given 'Ledger'.  The transactions are
-- retrieved using 'ledgerItems'.
allClatches :: Ledger l => (Amount -> Maybe Converted) -> l (Seq (Clatch l))
allClatches conv = do
  itms <- fmap join ledgerItems
  let txns = rights itms
  fmap join $ T.mapM (clatches conv) txns

-- | Map describing how different 'Commodity' are rendered.
newtype Renderings = Renderings
  (M.Map Commodity (NonEmpty (Arrangement, Either (Seq RadCom) (Seq RadPer))))
  deriving (Eq, Ord, Show)

-- | Given a Clatch, selects the best Amount from the original Amount
-- and the 'Converted' one.  If there is a 'Converted' amount, use
-- that; otherwise, use the original 'Amount'.
clatchAmount :: Clatch l -> Amount
clatchAmount (Clatch _ _ (Bevy _ amt mayConv) _)
  = maybe amt (\(Converted a) -> a) mayConv

-- | Given a Clatch, update the Renderings map.
updateRenderings :: Ledger l => Clatch l -> Renderings -> l Renderings
updateRenderings (Clatch _ _ (Bevy pstg _ _) _)
  (Renderings mp) = fmap f (postingTrio pstg)
  where
    f tri = case trioRendering tri of
      Nothing -> Renderings mp
      Just (cy, ar, ei) -> Renderings $ case M.lookup cy mp of
        Nothing -> M.insert cy (NonEmpty (ar, ei) S.empty) mp
        Just (NonEmpty o1 os) -> M.insert cy
          (NonEmpty o1 (os |> (ar, ei))) mp

-- | A 'Seq' of 'Slice' results from the filtering of a 'Seq' of
-- 'Clatch'.  Each 'Slice' is accompanied by a 'Serset'.
data Slice l = Slice (Clatch l) Serset

-- | Computes a series of 'Slice' from a series of 'Clatch' by
-- filtering using a given predicate.
slices
  :: Ledger l
  => PredM l (Clatch l)
  -> Seq (Clatch l)
  -> l (Seq (Slice l), Seq Result)
slices pd cltchs = do
  (withSers, rslt) <- serialedFilter pd cltchs
  return (fmap (uncurry Slice) withSers, rslt)

-- | A 'Seq' of 'Splint' is the result of the sorting of a 'Seq' of
-- 'Slice'.  Each 'Splint' has an accompanying 'Serset'.  Also, this
-- is the point at which 'Balances' are computed.  The 'Balances' is a
-- running balance, computed using the 'Amount' returned by
-- 'clatchAmount'.
data Splint l = Splint (Slice l) Serset Balances

splints
  :: Applicative f
  => (Seq (Slice l) -> f (Seq (Slice l)))
  -- ^ Sorter
  -> Seq (Slice l)
  -> f (Seq (Splint l))
splints srtr sq = fmap mkSplints $ srtr sq
  where
    mkSplints = addBals . serialNumbers
    addBals = snd . T.mapAccumL addBal mempty
    addBal bal (slce@(Slice clch _), srst) = (bal', splt)
      where
        bal' = addAmountToBalances (clatchAmount clch) bal
        splt = Splint slce srst bal'

-- | A 'Seq' of 'Tranche' is the result of the filtering of a 'Seq' of
-- 'Splint'.  Each 'Tranche' comes with a 'Serset'.
data Tranche l = Tranche (Splint l) Serset

tranches
  :: Applicative f
  => PredM f (Splint l)
  -> Seq (Splint l)
  -> f (Seq (Tranche l), Seq Result)
tranches pd
  = fmap (first (fmap (uncurry Tranche)))
  . serialedFilter pd

-- | Pulls together many functions in this module to deliver a quad
-- @(w, x, y, z)@, where @w@ is a list of all Tranche, @x@ is a list
-- of the 'Result' from filtering the 'Clatch', @y@ is the
-- 'Renderings', and @z@ is a list of the 'Result' from filtering the
-- 'Splint'.  The 'Clatch' are pulled ultimately by using
-- 'ledgerItems'.

allTranches
  :: Ledger l
  => (Amount -> Maybe Converted)
  -- ^ Converts the original 'Amount' to a different one.
  -> PredM l (Clatch l)
  -- ^ Filters 'Clatch'
  -> (Seq (Slice l) -> l (Seq (Slice l)))
  -- ^ Sorts 'Slice'
  -> PredM l (Splint l)
  -- ^ Filters 'Splint'
  -> l (Seq (Tranche l), Seq Result, Renderings, Seq Result)
allTranches conv pdCltch srtr pdSplint = do
  cltchs <- allClatches conv
  rndgs <- F.foldrM updateRenderings (Renderings M.empty) cltchs
  (slcs, rsltSlcs) <- slices pdCltch cltchs
  splnts <- splints srtr slcs
  (trchs, rsltTrchs) <- tranches pdSplint splnts
  return (trchs, rsltSlcs, rndgs, rsltTrchs)

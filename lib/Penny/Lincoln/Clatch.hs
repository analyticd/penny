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

import qualified Prednote as P
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
import Penny.Lincoln.Qty
import Data.Bifunctor
import qualified Control.Monad.Trans.State as St
import Control.Monad.Trans.Class
import Data.Functor.Contravariant
import qualified Data.Foldable as F

-- # Bevy

newtype Converted = Converted Amount

data Bevy l = Bevy (PostingL l) Amount (Maybe Converted)

-- # Clatches

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

bevies
  :: Ledger l
  => (Amount -> Maybe Converted)
  -> Seq (PostingL l)
  -> l (Seq (Bevy l))
bevies conv = T.traverse mkBevy
  where
    mkBevy pstg = f <$> postingQty pstg <*> postingCommodity pstg
      where
        f q c = Bevy pstg (Amount c q) (conv (Amount c q))

beviesToClatches
  :: TransactionL l
  -> Seq (Bevy l)
  -> Seq (Clatch l)
beviesToClatches txn = go S.empty
  where
    go onLeft onRight = case viewl onRight of
      EmptyL -> S.empty
      x :< xs -> Clatch txn onLeft x xs <| go (onLeft |> x) xs

clatches
  :: Ledger l
  => (Amount -> Maybe Converted)
  -> TransactionL l
  -> l (Seq (Clatch l))
clatches conv txn = do
  pstgs <- postings txn
  bvys <- bevies conv pstgs
  return $ beviesToClatches txn bvys

allClatches :: Ledger l => (Amount -> Maybe Converted) -> l (Seq (Clatch l))
allClatches conv = do
  itms <- fmap join ledgerItems
  let txns = rights itms
  fmap join $ T.mapM (clatches conv) txns

newtype Renderings = Renderings
  (M.Map Commodity (NonEmpty (Arrangement, Either (Seq RadCom) (Seq RadPer))))
  deriving (Eq, Ord, Show)

-- | Given a Clatch, selects the best Amount from the original Amount
-- and the 'Converted' one.
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

data Splint l = Splint (Slice l) Serset Balances

splints
  :: Applicative l
  => (Seq (Slice l) -> l (Seq (Slice l)))
  -- ^ Sorter
  -> Seq (Slice l)
  -> l (Seq (Splint l))
splints = undefined
{-
splints srtr sq = fmap mkSplints $ srtr sq
  where
    mkSplints = addBals . serialNumbers
    addBals = snd . T.mapAccumL addBal mempty
    addBal bal (slce@(Slice _ _ qty cy), srst) = (bal', splt)
      where
        bal' = addEntryToBalances qty cy bal
        splt = Splint slce srst bal'
-}
{-

splints
  :: Applicative l
  => (Seq (Slice l a) -> l (Seq (Slice l a)))
  -- ^ Sorter
  -> Seq (Slice l a)
  -> l (Seq (Splint l a))
splints srtr sq = fmap mkSplints $ srtr sq
  where
    mkSplints = addBals . serialNumbers
    addBals = snd . T.mapAccumL addBal mempty
    addBal bal (slce@(Slice _ _ qty cy), srst) = (bal', splt)
      where
        bal' = addEntryToBalances qty cy bal
        splt = Splint slce srst bal'

data Tranche l a = Tranche (Splint l a) Serset

tranches
  :: Applicative l
  => PredM l (Splint l a)
  -> Seq (Splint l a)
  -> l (Seq (Tranche l a), Seq Result)
tranches pd
  = fmap (first (fmap (uncurry Tranche)))
  . serialedFilter pd

-- | Pulls together many functions in this module to deliver a triple
-- @(x, y, z)@, where @x@ is a list of all Tranche, @y@ is a list of
-- the 'Result' from filtering the 'Clatch', and @z@ is a list of the
-- 'Result' from filtering the 'Splint'.  The 'Clatch' are pulled
-- ultimately by using 'ledgerItems'.

allTranches
  :: Ledger l
  => PredM l (Clatch l ())
  -- ^ Filters 'Clatch'
  -> (Seq (Slice l ()) -> l (Seq (Slice l ())))
  -- ^ Sorts 'Slice'
  -> PredM l (Splint l ())
  -- ^ Filters 'Splint'
  -> l (Seq (Tranche l ()), Seq Result, Renderings, Seq Result)
allTranches pdCltch srtr pdSplint = do
  cltchs <- allClatches
  (slcs, rsltSlcs, rndgs) <- slices pdCltch cltchs
  splnts <- splints srtr slcs
  (trchs, rsltTrchs) <- tranches pdSplint splnts
  return (trchs, rsltSlcs, rndgs, rsltTrchs)
-}

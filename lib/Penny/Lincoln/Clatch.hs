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

import Prednote
import Penny.Lincoln.Amount
import Penny.Lincoln.Ledger
import Penny.Lincoln.Filter
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

-- # Bevy

newtype Converted = Converted Amount

data Bevy l = Bevy (PostingL l) Amount (Maybe Converted)

-- # Clatches

data Clatch l a
  = Clatch (TransactionL l) (Seq (PostingL l, a))
           (PostingL l, a) (Seq (PostingL l, a))
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
  --
  -- Each posting @l@, @c@, and @r@ is paired with arbitrary metadata.

nextClatch :: Clatch l a -> Maybe (Clatch l a)
nextClatch (Clatch t l c r) = case viewl r of
  EmptyL -> Nothing
  x :< xs -> Just $ Clatch t (l |> c) x xs

prevClatch :: Clatch l a -> Maybe (Clatch l a)
prevClatch (Clatch t l c r) = case viewr l of
  EmptyR -> Nothing
  xs :> x -> Just $ Clatch t xs x (c <| r)

clatches :: Ledger l => TransactionL l -> l (Seq (Clatch l ()))
clatches txn = fmap (go S.empty . fmap (\a -> (a, ()))) $ postings txn
  where
    go onLeft onRight = case viewl onRight of
      EmptyL -> S.empty
      x :< xs -> curr <| rest
        where
          curr = Clatch txn onLeft x onRight
          rest = go (onLeft |> x) xs

allClatches :: Ledger l => l (Seq (Clatch l ()))
allClatches = do
  itms <- fmap join ledgerItems
  let txns = go itms
        where
          go sq = case S.viewl sq of
            EmptyL -> S.empty
            x :< xs -> case x of
              Left _ -> go xs
              Right txn -> txn <| go xs
  fmap join $ T.mapM clatches txns

newtype Renderings = Renderings
  (M.Map Commodity (NonEmpty (Arrangement, Either (Seq RadCom) (Seq RadPer))))
  deriving (Eq, Ord, Show)

-- | Takes a Clatch and pulls out its Qty and Commodity.  As a side
-- effect, updates the Renderings map.
clatchQtyAndCommodity
  :: Ledger l
  => Clatch l a
  -> St.StateT Renderings l (Qty, Commodity)
clatchQtyAndCommodity (Clatch _ _ pstg _) = do
  tri <- lift . postingTrio . fst $ pstg
  qty <- lift . postingQty . fst $ pstg
  case trioRendering tri of
    Nothing -> do
      cy <- lift . postingCommodity . fst $ pstg
      return (qty, cy)
    Just (cy, ar, ei) -> do
      Renderings mp <- St.get
      St.put . Renderings $ case M.lookup cy mp of
        Nothing -> M.insert cy (NonEmpty (ar, ei) S.empty) mp
        Just (NonEmpty o1 os) -> M.insert cy
          (NonEmpty o1 (os |> (ar, ei))) mp
      return (qty, cy)

data Slice l a = Slice (Clatch l a) Serset Qty Commodity

-- | Computes a series of 'Slice' from a series of 'Clatch' by
-- filtering using a given predicate.  As a side effect, generates a
-- set of 'Renderings' and the result of the filtering.
slices
  :: Ledger l
  => PredM l (Clatch l a)
  -> Seq (Clatch l a)
  -> l (Seq (Slice l a), Seq Result, Renderings)
slices pd cltchs = do
  (qtyCyPairs, rndgs) <- St.runStateT
    (T.traverse clatchQtyAndCommodity cltchs) (Renderings M.empty)
  (withSers, rslt) <- serialedFilter (contramap fst pd)
    (S.zip cltchs qtyCyPairs)
  let mkSlice ((cl, (qty, cy)), serset) = Slice cl serset qty cy
      slcs = fmap mkSlice withSers
  return (slcs, rslt, rndgs)


data Splint l a = Splint (Slice l a) Serset Balances

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

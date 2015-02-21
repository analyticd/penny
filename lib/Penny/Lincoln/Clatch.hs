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
import Penny.Lincoln.Ledger
import Penny.Lincoln.Filter
import Data.Sequence
  (Seq, viewl, ViewL(..), (|>), viewr, ViewR(..), (<|))
import qualified Data.Sequence as S
import qualified Data.Traversable as T
import qualified Data.Foldable as F
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

-- # Clatches

data Clatch l
  = Clatch (TransactionL l) (Seq (PostingL l))
           (PostingL l) (Seq (PostingL l))
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

clatches :: Ledger l => TransactionL l -> l (Seq (Clatch l))
clatches txn = fmap (go S.empty) $ postings txn
  where
    go onLeft onRight = case viewl onRight of
      EmptyL -> S.empty
      x :< xs -> curr <| rest
        where
          curr = Clatch txn onLeft x onRight
          rest = go (onLeft |> x) xs

allClatches :: Ledger l => l (Seq (Clatch l))
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

-- | Builds a map of all commodities and their corresponding radix
-- points and grouping characters.
renderingMap
  :: (Ledger l, F.Foldable f)
  => f (Clatch l)
  -> l (M.Map Commodity (NonEmpty (Either (Seq RadCom) (Seq RadPer))))
renderingMap = F.foldlM f M.empty
  where
    f mp (Clatch _ _ pstg _) = do
      tri <- postingTrio pstg
      return $ case trioRendering tri of
        Nothing -> mp
        Just (cy, ei) -> case M.lookup cy mp of
          Nothing -> M.insert cy (NonEmpty ei S.empty) mp
          Just (NonEmpty o1 os) -> M.insert cy (NonEmpty o1 (os |> ei)) mp

data Slice l = Slice (Clatch l) Serset Qty Commodity

slices
  :: Ledger l
  => PredM l (Clatch l)
  -> Seq (Clatch l)
  -> l (Seq (Slice l), Seq Result)
slices pd cltchs = do
  (withSers, rslt) <- serialedFilter pd cltchs
  let mkSlice (cl@(Clatch _ _ pstg _), srst) = do
        qty <- postingQty pstg
        cy <- postingCommodity pstg
        return $ Slice cl srst qty cy
  slcs <- T.traverse mkSlice withSers
  return (slcs, rslt)


data Splint l = Splint (Slice l) Serset Balances

splints
  :: Applicative l
  => (Seq (Slice l) -> l (Seq (Slice l)))
  -- ^ Sorter
  -> Seq (Slice l)
  -> l (Seq (Splint l))
splints srtr sq = fmap mkSplints $ srtr sq
  where
    mkSplints = addBals . serialNumbers
    addBals = snd . T.mapAccumL addBal mempty
    addBal bal (slce@(Slice _ _ qty cy), srst) = (bal', splt)
      where
        bal' = addEntryToBalances qty cy bal
        splt = Splint slce srst bal'

data Tranche l = Tranche (Splint l) Serset

tranches
  :: Applicative l
  => PredM l (Splint l)
  -> Seq (Splint l)
  -> l (Seq (Tranche l), Seq Result)
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
  => PredM l (Clatch l)
  -- ^ Filters 'Clatch'
  -> (Seq (Slice l) -> l (Seq (Slice l)))
  -- ^ Sorts 'Slice'
  -> PredM l (Splint l)
  -- ^ Filters 'Splint'
  -> l (Seq (Tranche l), Seq Result, Seq Result)
allTranches pdCltch srtr pdSplint = do
  cltchs <- allClatches
  (slcs, rsltSlcs) <- slices pdCltch cltchs
  splnts <- splints srtr slcs
  (trchs, rsltTrchs) <- tranches pdSplint splnts
  return (trchs, rsltSlcs, rsltTrchs)

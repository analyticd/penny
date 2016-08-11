{-# LANGUAGE RankNTypes #-}
-- | Creation of clatch-like types.

module Penny.Clatch.Create where

import Penny.Balance
import Penny.Clatch.Types
import Penny.Clatch.Access.Converted
import Penny.Converter
import Penny.Core
import Penny.Ents (Balanced, balancedToSeqEnt)
import Penny.SeqUtil
import Penny.Serial
import Penny.Tranche (Postline, TopLine)
import Penny.TransactionBare (TransactionBare(TransactionBare))
import Penny.Troika

import Control.Lens hiding (index)
import Control.Monad (join)
import Data.Bifunctor
import Data.Bifunctor.Flip
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import Data.Functor.Compose

createViewposts :: TransactionX l -> Seq (Sliced l ())
createViewposts txn = fmap (\vw -> (txn, (vw, ())))
  (allSlices . snd . snd $ txn)

-- | Applies a 'Converter' to convert a posting.
createConverted
  :: Converter
  -> Sliced l a
  -> Converted l ()
createConverted (Converter f) clatch = set (_2._2) (conv, ()) clatch
  where
    conv = f $ view amount clatch
    amount = _2._1.onSlice. Penny.Core.core. Penny.Core.troika . to c'Amount'Troika

createPrefilt
  :: (Converted l a -> Bool)
  -- ^ Predicate
  -> Seq (Converted l a)
  -> Seq (Prefilt l ())
createPrefilt pd
  = fmap arrange
  . serialNumbers
  . Seq.filter pd
  where
    arrange ((t, (v, (a, _))), s) = (t, (v, (a, (s, ()))))

createSortset
  :: (Prefilt l a -> Prefilt l a -> Ordering)
  -- ^ Sorter
  -> Seq (Prefilt l a)
  -> Seq (Sorted l ())
createSortset pd
  = fmap arrange
  . serialNumbers
  . Seq.sortBy pd
  where
    arrange ((t, (v, (a, (e, _)))), s) = (t, (v, (a, (e, (s, ())))))

addTotals
 :: forall l a. Seq (Sorted l a)
 -> Seq (Totaled l ())
addTotals = snd . T.mapAccumL f mempty
  where
    f bal clatch@(txn, (vw, (conv, (pf, (ss, _))))) =
      (bal', (txn, (vw, (conv, (pf, (ss, (bal', ())))))))
      where
        bal' = bal <> c'Balance'Amount (best clatch)

createClatch
  :: (Totaled  l() -> Bool)
  -- ^ Predicate
  -> Seq (Sorted l a)
  -> Seq (Clatch l)
createClatch pd
  = fmap arrange
  . serialNumbers
  . Seq.filter pd
  . addTotals
  where
    arrange ((t, (v, (a, (e, (s, (b, _)))))), o)
      = (t, (v, (a, (e, (s, (b, (o, ())))))))

--
-- # Adding serials
--

addSersets
  :: Seq (a, Seq b)
  -> Seq ((a, Serset), Seq (b, Serset))
addSersets
  = addTxn
  . addPstg
  where
    addTxn = fmap runFlip . getCompose . serialNumbers . Compose . fmap Flip
    addPstg = getCompose . getCompose . serialNumbers . Compose . Compose

addIndexes
  :: Balanced a
  -> Seq (Troika, a, Serset)
addIndexes
  = fmap (\((tm, trees), srst) -> (tm, trees, srst))
  . serialNumbers
  . balancedToSeqEnt

arrangeTransaction
  :: (((TopLine l, Serset), Serset),
      Seq (((Troika, Postline l, Serset), Serset), Serset))
  -> TransactionX l
arrangeTransaction (((txnMeta, txnLcl), txnGlbl), sq)
  = (Serpack txnLcl txnGlbl, (txnMeta, pstgs))
  where
    pstgs = fmap mkPstg sq
    mkPstg (((tm, trees, pstgIdx), pstgLcl), pstgGbl)
      = (Serpack pstgLcl pstgGbl, (trees, Core tm pstgIdx))

addSerials
  :: Seq (Seq (TransactionBare l))
  -> Seq (TransactionX l)
addSerials
  = fmap arrangeTransaction
  . addSersets
  . join
  . fmap (addSersets . fmap (second addIndexes))
  . fmap (fmap (\(TransactionBare tl pstgs) -> (tl, pstgs)))


--
-- Creator
--

clatchesFromTransactions
  :: Converter
  -- ^ Converts amounts
  -> (Converted l () -> Bool)
  -- ^ Filters 'Converted'
  -> (Prefilt l () -> Prefilt l () -> Ordering)
  -- ^ Sorts 'Prefilt'
  -> (Totaled l () -> Bool)
  -- ^ Filters 'Totaled'
  -> Seq (TransactionX l)
  -> Seq (Clatch l)
clatchesFromTransactions converter pConverted sorter pTotaled
  = createClatch pTotaled
  . createSortset sorter
  . createPrefilt pConverted
  . fmap (createConverted converter)
  . join
  . fmap createViewposts

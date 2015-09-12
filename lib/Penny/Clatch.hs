{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Penny.Clatch
  ( -- * Postings
    Core(..)
  , troimount
  , index

  , Posting
  , core
  , postings

  -- * Transactions
  , Transaction

  -- * Functions on postings and transactions
  , serpack
  , trees

  -- * Sersets
  , PreFiltset
  , Sortset
  , PostFiltset

  -- * Clatches and compatible types
  , Viewed
  , Converted
  , Prefilt
  , Sorted
  , Totaled
  , Clatch

  -- * Functions on clatches and compatible types
  , transaction
  , view
  , converted
  , best
  , preFiltset
  , sortset
  , balance
  , postFiltset

  -- * Creation of clatches
  , addSerials
  , clatchesFromTransactions
  ) where

import Control.Lens hiding (view, index)
import Control.Monad (join)
import Data.Bifunctor
import Data.Bifunctor.Flip
import Data.Monoid
import Penny.Amount
import Penny.Converter
import Penny.Balance
import Penny.Ents (Balanced, balancedToSeqEnt)
import Penny.Tree
import Penny.Troika
import Penny.Serial
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.SeqUtil
import qualified Data.Traversable as T
import Data.Functor.Compose

-- | The core of every posting.
data Core = Core
  { _troimount :: Troika
  , _index :: Serset
  -- ^ How this single posting relates to its sibling postings.
  -- Numbering restarts with every transaction.
  } deriving Show

makeLenses ''Core

-- | A posting, coupled with metadata in the form of 'Tree' and a
-- 'Serpack' that indicates how this posting relates to other
-- postings.
type Posting = (Serpack, (Seq Tree, Core))

-- | A list of postings, coupled with metadata in the form of 'Tree'
-- and with a 'Serpack' that indicates how this transaction relates to
-- other transactions.
type Transaction = (Serpack, (Seq Tree, Seq Posting))

-- # Functions on postings and transactions

core :: Posting -> Core
core = snd . snd

postings :: Transaction -> Seq Posting
postings = snd . snd

-- |
-- @
-- 'serpack' :: 'Transaction' -> 'Serpack'
-- 'serpack' :: 'Posting' -> 'Serpack'
-- @
serpack :: (Serpack, a) -> Serpack
serpack = fst

-- |
-- @
-- 'trees' :: 'Transaction' -> 'Seq' 'Tree'
-- 'trees' :: 'Posting' -> 'Seq' 'Tree'
-- @
trees :: (a, (Seq Tree, b)) -> Seq Tree
trees = fst . snd

-- | The 'Serset' after all postings have been pre-filtered.
type PreFiltset = Serset

-- | The 'Serset' after all views have been sorted.
type Sortset = Serset

-- | The 'Serset' after the sorted views have been post-filtered.
type PostFiltset = Serset

-- # Clatches and compatible types

type Viewed a = (Transaction, (View Posting, a))
type Converted a = (Transaction, (View Posting, (Maybe Amount, a)))
type Prefilt a = (Transaction, (View Posting, (Maybe Amount, (PreFiltset, a))))
type Sorted a = (Transaction, (View Posting, (Maybe Amount, (PreFiltset,
                  (Sortset, a)))))
type Totaled a = (Transaction, (View Posting, (Maybe Amount, (PreFiltset,
                   (Sortset, (Balance, a))))))

type Clatch =
  (Transaction, (View Posting, (Maybe Amount, (PreFiltset, (Sortset,
    (Balance, (PostFiltset, ())))))))

-- # Functions on clatches

transaction :: (Transaction, a) -> Transaction
transaction = fst

view :: (a, (View Posting, b)) -> View Posting
view = fst . snd

converted :: (a, (b, (Maybe Amount, c))) -> Maybe Amount
converted = fst . snd . snd

best :: (a, (View Posting, (Maybe Amount, c))) -> Amount
best clatch = case converted clatch of
  Just a -> a
  Nothing -> clatch ^. to view . onView . to core
    . troimount . to c'Amount'Troika

preFiltset :: (a, (b, (c, (PreFiltset, d)))) -> PreFiltset
preFiltset = fst . snd . snd . snd

sortset :: (a, (b, (c, (d, (Sortset, e))))) -> Sortset
sortset = fst . snd . snd . snd . snd

balance :: (a, (b, (c, (d, (e, (Balance, f)))))) -> Balance
balance = fst . snd . snd . snd . snd . snd

postFiltset :: (a, (b, (c, (d, (e, (f, (PostFiltset, g))))))) -> PostFiltset
postFiltset = fst . snd . snd . snd . snd . snd . snd

--
-- # Creation
--

createViewposts :: Transaction -> Seq (Viewed ())
createViewposts txn = fmap (\vw -> (txn, (vw, ())))
  (allViews . snd . snd $ txn)

-- | Applies a 'Converter' to convert a posting.
createConverted
  :: Converter
  -> Viewed a
  -> Converted ()
createConverted (Converter f) clatch = clatch & _2._2 .~ (conv, ())
  where
    conv = f $ clatch ^. _2._1.onView.to core. troimount . to c'Amount'Troika

createPrefilt
  :: (Converted a -> Bool)
  -- ^ Predicate
  -> Seq (Converted a)
  -> Seq (Prefilt ())
createPrefilt pd
  = fmap arrange
  . serialNumbers
  . Seq.filter pd
  where
    arrange ((t, (v, (a, _))), s) = (t, (v, (a, (s, ()))))

createSortset
  :: (Prefilt a -> Prefilt a -> Ordering)
  -- ^ Sorter
  -> Seq (Prefilt a)
  -> Seq (Sorted ())
createSortset pd
  = fmap arrange
  . serialNumbers
  . Seq.sortBy pd
  where
    arrange ((t, (v, (a, (e, _)))), s) = (t, (v, (a, (e, (s, ())))))

addTotals
  :: Seq (Sorted a)
  -> Seq (Totaled ())
addTotals = snd . T.mapAccumL f mempty
  where
    f bal clatch@(txn, (vw, (conv, (pf, (ss, _))))) =
      (bal', (txn, (vw, (conv, (pf, (ss, (bal', ())))))))
      where
        bal' = bal <> c'Balance'Amount (best clatch)

createClatch
  :: (Totaled () -> Bool)
  -- ^ Predicate
  -> Seq (Sorted a)
  -> Seq Clatch
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
  :: Balanced (Seq Tree)
  -> Seq (Troika, Seq Tree, Serset)
addIndexes
  = fmap (\((tm, trees), srst) -> (tm, trees, srst))
  . serialNumbers
  . balancedToSeqEnt

arrangeTransaction
  :: (((Seq Tree, Serset), Serset),
      Seq (((Troika, Seq Tree, Serset), Serset), Serset))
  -> Transaction
arrangeTransaction (((txnMeta, txnLcl), txnGlbl), sq)
  = (Serpack txnLcl txnGlbl, (txnMeta, pstgs))
  where
    pstgs = fmap mkPstg sq
    mkPstg (((tm, trees, pstgIdx), pstgLcl), pstgGbl)
      = (Serpack pstgLcl pstgGbl, (trees, Core tm pstgIdx))

addSerials
  :: Seq (Seq (Seq Tree, Balanced (Seq Tree)))
  -> Seq Transaction
addSerials
  = fmap arrangeTransaction
  . addSersets
  . join
  . fmap addSersets
  . fmap (fmap (second addIndexes))

--
-- Creator
--


clatchesFromTransactions
  :: Converter
  -> (Converted () -> Bool)
  -> (Prefilt () -> Prefilt () -> Ordering)
  -> (Totaled () -> Bool)
  -> Seq Transaction
  -> Seq Clatch
clatchesFromTransactions converter pConverted sorter pTotaled
  = createClatch pTotaled
  . createSortset sorter
  . createPrefilt pConverted
  . fmap (createConverted converter)
  . join
  . fmap createViewposts

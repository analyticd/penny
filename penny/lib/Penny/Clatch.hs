{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Types that group a posting with other interesting information.
module Penny.Clatch
  ( -- * Postings
    Core(..)
  , troika
  , birth

  , Posting
  , core
  , postings

  -- * Transactions
  , Transaction

  -- * Functions on postings and transactions
  , Penny.Clatch.serpack
  , tranche

  -- * Sersets
  , PreFiltset
  , Sortset
  , PostFiltset

  -- * Clatches and compatible types

  -- | These types are designed so that various functions and lenses
  -- can operate on values of multiple types.
  , Sliced
  , Converted
  , Prefilt
  , Sorted
  , Totaled
  , Clatch

  -- * Lenses and Functions on clatches and compatible types
  , transaction
  , slice
  , posting
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

import Penny.Amount
import Penny.Balance
import Penny.Converter
import Penny.Copper.Decopperize
import Penny.Ents (Balanced, balancedToSeqEnt)
import Penny.SeqUtil
import Penny.Serial
import Penny.Tranche
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

-- | The core of every posting.
data Core = Core
  { _troika :: Troika
  , _birth :: Serset
  -- ^ How this single posting relates to its sibling postings.
  -- Numbering restarts with every transaction.
  } deriving Show

makeLenses ''Core

-- | A posting, coupled with metadata in the form of 'Tree' and a
-- 'Serpack' that indicates how this posting relates to other
-- postings.
type Posting a = (Serpack, (Postline a, Core))

-- | A list of postings, coupled with metadata in the form of 'Tree'
-- and with a 'Serpack' that indicates how this transaction relates to
-- other transactions.
type Transaction a = (Serpack, (TopLine a, Seq (Posting a)))

-- # Functions on postings and transactions

core :: Lens' (Posting a) Core
core = _2 . _2

postings :: Lens' (Transaction a) (Seq (Posting a))
postings = _2 . _2

-- |
-- @
-- 'serpack' :: 'Lens'' 'Transaction' 'Serpack'
-- 'serpack' :: 'Lens'' 'Posting' 'Serpack'
-- @
serpack :: Lens' (Serpack, a) Serpack
serpack = _1

-- |
-- @
-- 'tranche' :: 'Lens'' ('Transaction' a) ('TopLine' a)
-- 'tranche' :: 'Lens'' ('Posting' a) ('Postline' a)
-- @
tranche :: forall a b c. Lens' (Serpack, (Tranche b a, c)) (Tranche b a)
tranche = _2 . _1

-- | The 'Serset' after all postings have been pre-filtered.
type PreFiltset = Serset

-- | The 'Serset' after all slices have been sorted.
type Sortset = Serset

-- | The 'Serset' after the sorted slices have been post-filtered.
type PostFiltset = Serset

-- # Clatches and compatible types

-- | A single 'Slice' 'Posting' contains not only the 'Posting' but
-- also all sibling 'Posting's.  A 'Transaction' can give rise to
-- multiple 'Slice's, and therefore to mulitple 'Sliced'.
type Sliced l a = (Transaction l, (Slice (Posting l), a))

-- | After 'Sliced' are created, the posting's 'Amount' is converted
-- using the specified 'Converter'.  There might not be any conversion
-- if the 'Converter' does not perform one.
type Converted l a = (Transaction l, (Slice (Posting l), (Maybe Amount, a)))

-- | After 'Converted' are created, they are filtered.  After
-- filtering, a 'PreFiltset' is assigned.
type Prefilt l a = (Transaction l, (Slice (Posting l),
  (Maybe Amount, (PreFiltset, a))))

-- | After the 'Prefilt' are created, they are sorted.  After sorting
-- a 'Sortset' is assigned.
type Sorted l a = (Transaction l, (Slice (Posting l), (Maybe Amount,
  (PreFiltset, (Sortset, a)))))

-- | After the 'Sorted' are created, the running balance is calculated
-- for each 'Sorted'.
type Totaled l a = (Transaction l, (Slice (Posting l),
  (Maybe Amount, (PreFiltset, (Sortset, (Balance, a))))))

-- | After 'Totaled' are created, they are filtered.  After filtering
-- a 'PostFiltset' is assigned.
type Clatch l =
  (Transaction l, (Slice (Posting l), (Maybe Amount, (PreFiltset, (Sortset,
    (Balance, (PostFiltset, ())))))))

-- # Lenses and functions on clatches

-- | Operates on the original 'Transaction'.
--
-- @
-- 'transaction' :: 'Lens'' ('Sliced' a)    'Transaction'
-- 'transaction' :: 'Lens'' ('Converted' a) 'Transaction'
-- 'transaction' :: 'Lens'' ('Prefilt' a)   'Transaction'
-- 'transaction' :: 'Lens'' ('Sorted' a)    'Transaction'
-- 'transaction' :: 'Lens'' ('Totaled' a)   'Transaction'
-- 'transaction' :: 'Lens'' 'Clatch'        'Transaction'
-- @

transaction :: Lens' (Transaction l, a) (Transaction l)
transaction = _1

-- | Operate on the 'Slice'.
--
-- @
-- 'slice' :: 'Lens'' ('Sliced' a)    ('Slice' 'Posting')
-- 'slice' :: 'Lens'' ('Converted' a) ('Slice' 'Posting')
-- 'slice' :: 'Lens'' ('Prefilt' a)   ('Slice' 'Posting')
-- 'slice' :: 'Lens'' ('Sorted' a)    ('Slice' 'Posting')
-- 'slice' :: 'Lens'' ('Totaled' a)   ('Slice' 'Posting')
-- 'slice' :: 'Lens'' 'Clatch'        ('Slice' 'Posting')
-- @

slice :: Lens' (a, (Slice (Posting l), b)) (Slice (Posting l))
slice = _2 . _1

-- | Operates on the 'Posting' in the 'Slice'.
--
-- @
-- 'posting' :: 'Lens'' ('Sliced' a)    'Posting'
-- 'posting' :: 'Lens'' ('Converted' a) 'Posting'
-- 'posting' :: 'Lens'' ('Prefilt' a)   'Posting'
-- 'posting' :: 'Lens'' ('Sorted' a)    'Posting'
-- 'posting' :: 'Lens'' ('Totaled' a)   'Posting'
-- 'posting' :: 'Lens'' 'Clatch'        'Posting'
-- @

posting :: Lens' (a, (Slice (Posting l), b)) (Posting l)
posting = slice . onSlice

-- | Operate on the converted 'Amount'.  There is no converted
-- 'Amount' if the 'Converter' did not specify a conversion.
--
-- @
-- 'converted' :: 'Lens'' ('Converted' a) ('Maybe' 'Amount')
-- 'converted' :: 'Lens'' ('Prefilt' a)   ('Maybe' 'Amount')
-- 'converted' :: 'Lens'' ('Sorted' a)    ('Maybe' 'Amount')
-- 'converted' :: 'Lens'' ('Totaled' a)   ('Maybe' 'Amount')
-- 'converted' :: 'Lens'' 'Clatch'        ('Maybe' 'Amount')
-- @

converted :: Lens' (a, (b, (Maybe Amount, c))) (Maybe Amount)
converted = _2 . _2 . _1

-- | If there is a converted amount, then use that.  Otherwise, use
-- the original, unconverted amount.  This is not a 'Lens' but it is a
-- 'Getter'.
--
-- @
-- 'best' :: 'Getter' ('Converted' a) 'Amount'
-- 'best' :: 'Getter' ('Prefilt' a)   'Amount'
-- 'best' :: 'Getter' ('Sorted' a)    'Amount'
-- 'best' :: 'Getter' ('Totaled' a)   'Amount'
-- 'best' :: 'Getter' 'Clatch'        'Amount'
-- @

best :: Getter (a, (Slice (Posting l), (Maybe Amount, c))) Amount
best = to $ \clatch -> case view converted clatch of
  Just a -> a
  Nothing -> clatch ^. slice . onSlice . core
    . troika . to c'Amount'Troika

-- | Operate on the 'PreFiltset'.
--
-- @
-- 'preFiltset' :: 'Lens'' ('Prefilt' a) 'PreFiltset'
-- 'preFiltset' :: 'Lens'' ('Sorted' a)  'PreFiltset'
-- 'preFiltset' :: 'Lens'' ('Totaled' a) 'PreFiltset'
-- 'preFiltset' :: 'Lens'' 'Clatch'      'PreFiltset'
-- @

preFiltset :: Lens' (a, (b, (c, (PreFiltset, d)))) PreFiltset
preFiltset = _2 . _2 . _2 . _1

-- | Operate on the 'Sortset'.
--
-- @
-- 'sortset' :: 'Lens'' ('Sorted' a)  'Sortset'
-- 'sortset' :: 'Lens'' ('Totaled' a) 'Sortset'
-- 'sortset' :: 'Lens'' 'Clatch'      'Sortset'
-- @

sortset :: Lens' (a, (b, (c, (d, (Sortset, e))))) Sortset
sortset = _2 . _2 . _2 . _2 . _1

-- | Operate on the 'Balance'.
--
-- @
-- 'balance' :: 'Lens'' ('Totaled' a) 'Balance'
-- 'balance' :: 'Lens'' 'Clatch'      'Balance'
-- @

balance :: Lens' (a, (b, (c, (d, (e, (Balance, f)))))) Balance
balance = _2 . _2 . _2 . _2 . _2 . _1

-- | Operate on the 'PostFiltset'.
--
-- @
-- 'postFiltset' :: 'Lens'' 'Clatch' 'PostFiltset'
-- @
postFiltset :: Lens' (a, (b, (c, (d, (e, (f, (PostFiltset, g))))))) PostFiltset
postFiltset =  _2 . _2 . _2 . _2 . _2 . _2 . _1

--
-- # Creation
--

createViewposts :: Transaction l -> Seq (Sliced l ())
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
    amount = _2._1.onSlice.core. troika . to c'Amount'Troika

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
        bal' = bal <> c'Balance'Amount (view best clatch)

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
  -> Transaction l
arrangeTransaction (((txnMeta, txnLcl), txnGlbl), sq)
  = (Serpack txnLcl txnGlbl, (txnMeta, pstgs))
  where
    pstgs = fmap mkPstg sq
    mkPstg (((tm, trees, pstgIdx), pstgLcl), pstgGbl)
      = (Serpack pstgLcl pstgGbl, (trees, Core tm pstgIdx))

addSerials
  :: Seq (Seq (TopLine l, Balanced (Postline l)))
  -> Seq (Transaction l)
addSerials
  = fmap arrangeTransaction
  . addSersets
  . join
  . fmap (addSersets . fmap (second addIndexes))


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
  -> Seq (Transaction l)
  -> Seq (Clatch l)
clatchesFromTransactions converter pConverted sorter pTotaled
  = createClatch pTotaled
  . createSortset sorter
  . createPrefilt pConverted
  . fmap (createConverted converter)
  . join
  . fmap createViewposts

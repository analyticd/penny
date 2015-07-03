{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Penny.Clatch where

import Control.Lens hiding (view)
import Data.Monoid
import Penny.Amount
import Penny.Converter
import Penny.Balance
import Penny.DateTime
import Penny.Tree
import Penny.Trio
import Penny.Qty
import Penny.Commodity
import Penny.Serial
import Penny.Exch
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Penny.Price as Price
import Penny.SeqUtil
import qualified Data.Traversable as T

data Core = Core
  { _trio :: Trio
  , _amount :: Amount
  , _index :: Serset
  } deriving (Eq, Ord, Show)

makeLenses ''Core

type Posting = (Serpack, (Seq Tree, Core))
type Transaction = (Serpack, (Seq Tree, Seq Posting))

-- # Functions on postings and transactions

core :: Posting -> Core
core = snd . snd

postings :: Transaction -> Seq Posting
postings = snd . snd

serpack :: (Serpack, a) -> Serpack
serpack = fst

trees :: (a, (Seq Tree, b)) -> Seq Tree
trees = fst . snd

type PreFiltset = Serset
type Sortset = Serset
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
  Nothing -> clatch ^. to view . onView . to core . amount

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

createViewposts :: Transaction -> Seq (Transaction, (View Posting, ()))
createViewposts txn = fmap (\vw -> (txn, (vw, ())))
  (allViews . snd . snd $ txn)

-- | Applies a 'Converter' to convert a posting.
createConverted
  :: Converter
  -> Viewed a
  -> Converted ()
createConverted (Converter f) clatch = clatch & _2._2 .~ (conv, ())
  where
    conv = f $ clatch ^. _2._1.onView.to core.amount

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
  :: (Prefilt a -> Bool)
  -- ^ Predicate
  -> Seq (Prefilt a)
  -> Seq (Sorted ())
createSortset pd
  = fmap arrange
  . serialNumbers
  . Seq.filter pd
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

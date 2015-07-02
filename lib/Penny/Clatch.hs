{-# LANGUAGE TemplateHaskell #-}
module Penny.Clatch where

import Control.Lens
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
import qualified Penny.Price as Price
import Penny.SeqUtil

data Price = Price
  { _dateTime :: DateTime
  , _fromTo :: Price.FromTo
  , _exch :: Exch
  } deriving (Eq, Ord, Show)

makeLenses ''Price

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

createConverted
  :: Converter
  -> (a, (View Posting, b))
  -> (a, (View Posting, (Maybe Amount, ())))
createConverted (Converter f) clatch = clatch & _2._2 .~ (conv, ())
  where
    conv = f $ clatch ^. _2._1.onView.to core.amount

createPrefilt
  :: ((Transaction, (View Posting, (Maybe Amount, ()))) -> Bool)
  -- ^ Predicate
  -> Seq (Transaction, (View Posting, (Maybe Amount, ())))
  -> Seq (Transaction, (View Posting, (Maybe Amount, (PreFiltset, ()))))
createPrefilt = undefined

{-# LANGUAGE TypeFamilies #-}

-- | A 'Ledger' is a store of transactions and prices.  'Ledger'
-- specifies an interface for such stores.
module Penny.Ledger where

import Control.Monad
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.DateTime
import Penny.Lincoln.Commodity
import Penny.Lincoln.Prices hiding (fromTo)
import Penny.Lincoln.Exch
import Data.Sequence (Seq, ViewL(..), viewl)
import qualified Data.Sequence as S
import Penny.Lincoln.Transaction
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as X
import Control.Monad.Reader
import Penny.Matcher

class Monad l => Ledger l where
  type PriceL l
  type TransactionL l
  type TreeL l
  type PostingL l

  ------------------------------------------------
  -- All items
  ------------------------------------------------

  -- | All the items contained in the Ledger.
  vault :: l (Seq (Seq (Either (PriceL l) (TransactionL l))))

  ------------------------------------------------
  -- Prices
  ------------------------------------------------

  -- | When this price became effective
  dateTime :: PriceL l -> l DateTime

  -- | 1 unit of the from commodity equals the given number of
  -- exchange commodity
  fromTo :: PriceL l -> l FromTo

  -- | 1 unit of the from commodity in the 'trade' equals this much of
  -- the to commodity in the 'trade'
  exchange :: PriceL l -> l Exch

  ------------------------------------------------
  -- Trees
  ------------------------------------------------

  -- | Information held in this node of the tree.
  scalar :: TreeL l -> l (Maybe Scalar)

  -- | Each tree is in a particular realm.
  realm :: TreeL l -> l Realm

  -- | Child trees of a particular tree.
  offspring :: TreeL l -> l (Seq (TreeL l))


  ------------------------------------------------
  -- Transactions
  ------------------------------------------------

  -- | The metadata for the transaction.
  txnMeta :: TransactionL l -> l (Seq (TreeL l))

  -- | The serial that applies to the entire transction.
  topLineSer :: TransactionL l -> l TopLineSer

  -- | All postings.  A posting that is in a transaction is a @plink@.
  postings :: TransactionL l -> l (Seq (PostingL l))

  ------------------------------------------------
  -- Postings
  ------------------------------------------------
  -- | Metadata for a single plink.
  pstgMeta :: PostingL l -> l (Seq (TreeL l))

  -- | The Trio that belongs to a posting.
  trio :: PostingL l -> l Trio

  -- | The quantity that belongs to a posting.
  qty :: PostingL l -> l Qty

  -- | The unit of currency for the posting.
  commodity :: PostingL l -> l Commodity

  -- | The serial that belongs to a posting.
  postingSer :: PostingL l -> l PostingSer

instance Ledger m => Ledger (Matcher t m) where
  type PriceL (Matcher t m) = PriceL m
  type TransactionL (Matcher t m) = TransactionL m
  type TreeL (Matcher t m) = TreeL m
  type PostingL (Matcher t m) = PostingL m

  vault = lift vault
  dateTime = lift . dateTime
  fromTo = lift . fromTo
  exchange = lift . exchange
  scalar = lift . scalar
  realm = lift . realm
  offspring = lift . offspring
  txnMeta = lift . txnMeta
  topLineSer = lift . topLineSer
  postings = lift . postings
  pstgMeta = lift . pstgMeta
  trio = lift . trio
  qty = lift . qty
  commodity = lift . commodity
  postingSer = lift . postingSer


-- # Displaying trees

-- To deal with special Unicode characters in Emacs, use C-x 8 to
-- insert them.  C-x = will give brief information about the character
-- at point; M-x describe-char gives more detailed information.

-- | Displays a tree.  It's impractical to display any of the children
-- of the child trees, so any child tree that has children is suffixed
-- with a down arrow (which is U+2193, or ↓) to let the user know
-- something is down there.

displayTreeL
  :: Ledger l
  => TreeL l
  -> l Text
displayTreeL t = liftM2 f (scalar t) (offspring t)
  where
    f sc cs = maybe X.empty displayScalar sc <>
      if S.null cs then mempty else X.singleton '↓'

-- | Displays a forest of trees, with each separated by a bullet
-- (which is U+2022, or •).
displayForestL
  :: Ledger l
  => Seq (TreeL l)
  -> l Text
displayForestL sq = case viewl sq of
  EmptyL -> return X.empty
  x1 :< xs1 -> do
    t1 <- displayTreeL x1
    let dispNext t = liftM (X.cons '•') $ displayTreeL t
    liftM (F.foldl' mappend t1) $ T.mapM dispNext xs1


{-# LANGUAGE TypeFamilies #-}

-- | A 'Ledger' is a store of transactions and prices.  'Ledger'
-- specifies an interface for such stores.
module Penny.Lincoln.Ledger where

import Control.Monad
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.DateTime
import Penny.Lincoln.Commodity
import Penny.Lincoln.Prices
import Penny.Lincoln.Exch
import Penny.Lincoln.Matcher
import Data.Sequence (Seq, ViewL(..), viewl)
import qualified Data.Sequence as S
import Penny.Lincoln.Transaction
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as X
import Control.Monad.Reader

class Monad l => Ledger l where
  type PriceL (l :: * -> *) :: *
  type TransactionL (l :: * -> *) :: *
  type TreeL (l :: * -> *) :: *
  type PostingL (l :: * -> *) :: *

  ------------------------------------------------
  -- All items
  ------------------------------------------------

  -- | All the items contained in the Ledger.
  vault :: l (Seq (Seq (Either (PriceL l) (TransactionL l))))

  ------------------------------------------------
  -- Prices
  ------------------------------------------------

  -- | When this price became effective
  instant :: PriceL l -> l DateTime

  -- | 1 unit of the from commodity equals the given number of
  -- exchange commodity
  trade :: PriceL l -> l FromTo

  -- | 1 unit of the from commodity in the 'trade' equals this much of
  -- the to commodity in the 'trade'
  exchange :: PriceL l -> l Exch

  ------------------------------------------------
  -- Trees
  ------------------------------------------------

  -- | Information held in this node of the tree.
  capsule :: TreeL l -> l (Maybe Scalar)

  -- | Each tree is in a particular namespace.
  namespace :: TreeL l -> l Realm

  -- | Child trees of a particular tree.
  offspring :: TreeL l -> l (Seq (TreeL l))


  ------------------------------------------------
  -- Transactions
  ------------------------------------------------

  -- | The metadata for the transaction.
  txnMeta :: TransactionL l -> l (Seq (TreeL l))

  -- | The serial that applies to the entire transction.
  zonk :: TransactionL l -> l TopLineSer

  -- | All postings.  A posting that is in a transaction is a @plink@.
  plinks :: TransactionL l -> l (Seq (PostingL l))

  ------------------------------------------------
  -- Plinks
  ------------------------------------------------
  -- | Metadata for a single plink.
  plinkMeta :: PostingL l -> l (Seq (TreeL l))

  -- | The Trio that belongs to a posting.
  triplet :: PostingL l -> l Trio

  -- | The quantity that belongs to a posting.
  quant :: PostingL l -> l Qty

  -- | The unit of currency for the posting.
  curren :: PostingL l -> l Commodity

  -- | The serial that belongs to a posting.
  xylo :: PostingL l -> l PostingSer

instance Ledger m => Ledger (Matcher t m) where
  type PriceL (Matcher t m) = PriceL m
  type TransactionL (Matcher t m) = TransactionL m
  type TreeL (Matcher t m) = TreeL m
  type PostingL (Matcher t m) = PostingL m

  vault = lift vault
  instant = lift . instant
  trade = lift . trade
  exchange = lift . exchange
  capsule = lift . capsule
  namespace = lift . namespace
  offspring = lift . offspring
  txnMeta = lift . txnMeta
  zonk = lift . zonk
  plinks = lift . plinks
  plinkMeta = lift . plinkMeta
  triplet = lift . triplet
  quant = lift . quant
  curren = lift . curren
  xylo = lift . xylo


-- # Displaying trees

-- To deal with special Unicode characters in Emacs, use C-x 8 to
-- insert them.  C-x = will give brief information about the character
-- at point; M-x describe-char gives more detailed information.

-- | Displays a tree.  It's impractical to display any of the children
-- of the child trees, so any child tree that has children is suffixed
-- with a down arrow (which is U+2193, or ↓) to let the user know
-- something is down there.

displayTree
  :: Ledger l
  => TreeL l
  -> l Text
displayTree t = liftM2 f (capsule t) (offspring t)
  where
    f sc cs = maybe X.empty displayScalar sc <>
      if S.null cs then mempty else X.singleton '↓'

-- | Displays a forest of trees, with each separated by a bullet
-- (which is U+2022, or •).
displayForest
  :: Ledger l
  => Seq (TreeL l)
  -> l Text
displayForest sq = case viewl sq of
  EmptyL -> return X.empty
  x1 :< xs1 -> do
    t1 <- displayTree x1
    let dispNext t = liftM (X.cons '•') $ displayTree t
    liftM (F.foldl' mappend t1) $ T.mapM dispNext xs1


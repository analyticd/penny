{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A 'Scroll' is a an implementation of a 'Ledger'.  The 'Scroll' is
-- the simplest 'Ledger', as it simply takes a list of concrete prices
-- and transactions and stores them in memory.
module Penny.Lincoln.Ledger.Scroll where

import Penny.Lincoln.Ledger
import Control.Applicative
import Control.Monad.Trans.Class
import Penny.Lincoln.Transaction
import Data.Functor.Identity
import Control.Monad
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.Commodity
import Penny.Lincoln.Ents
import qualified Data.Foldable as Fdbl
import Penny.Lincoln.Ent
import Penny.Lincoln.Prices
import Control.Monad.Reader
import Data.Sequence (Seq)

type Topload = (Seq Tree, TopLineSer)
type Plinkload = (Seq Tree, Trio, Qty, Commodity, PostingSer)
type Environment
  = [[Either Price (Transaction Topload Plinkload)]]

newtype ScrollT m a
  = ScrollT (ReaderT Environment m a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadTrans
  , MonadReader Environment
  )

type Scroll = ScrollT Identity

{-

data Posting = Posting [Tree] Trio Qty Commodity
  deriving (Eq, Ord, Show)

balancedToPostings :: Balanced PstgMeta -> [Posting]
balancedToPostings = Fdbl.toList . fmap f . balancedToSeqEnt
  where
    f (Ent q cy (PstgMeta ts tri)) = Posting ts tri q cy
-}
{-
instance (Applicative m, Monad m) => Ledger (ScrollT m) where
  type PriceL (ScrollT m) = Price
  type TransactionL (ScrollT m) = Transaction
  type TreeL (ScrollT m) = Tree
  type PostingL (ScrollT m) = Posting

  ledgerItems = (ScrollT return)

  priceDate (Price d _ _) = ScrollT . const . return $ d
  priceFromTo (Price _ ft _) = ScrollT . const . return $ ft
  priceExch (Price _ _ e) = ScrollT . const . return $ e

  transactionMeta (Transaction (TopLine ts) _) = ScrollT . const . return $ ts
  scalar (Tree _ n _) = ScrollT . const . return $ n
  realm (Tree r _ _) = ScrollT . const . return $ r
  children (Tree _ _ cs) = ScrollT . const . return $ cs
  postings (Transaction _ bal) = ScrollT . const
    . return . balancedToPostings $ bal
  postingTrees (Posting ts _ _ _) = ScrollT . const . return $ ts
  postingTrio (Posting _ tr _ _) = ScrollT . const . return $ tr
  postingQty (Posting _ _ q _) = ScrollT . const . return $ q
  postingCommodity (Posting _ _ _ cy) = ScrollT . const . return $ cy


-}

{-# LANGUAGE OverloadedStrings #-}

module Penny.Ledger.Matcher where

import Control.Applicative
import qualified Data.Foldable as F
import Control.Monad.Trans.Class
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.Exch
import Penny.Ledger
import Penny.DateTime
import Penny.Matcher
import Penny.Price
import Penny.Field
import Penny.Transaction
import Penny.Trio
import Penny.Qty
import Penny.Commodity

-- # Prices

dateTime
  :: Ledger m
  => Matcher DateTime m a
  -> Matcher (PriceL m) m a
dateTime = labelNest "dateTime" Penny.Ledger.dateTime

fromTo
  :: Ledger m
  => Matcher FromTo m a
  -> Matcher (PriceL m) m a
fromTo = labelNest "fromTo" Penny.Ledger.fromTo

exchange
  :: Ledger m
  => Matcher Exch m a
  -> Matcher (PriceL m) m a
exchange = labelNest "exchange" Penny.Ledger.exchange


scalar
  :: Ledger m
  => Matcher Scalar m a
  -> Matcher (TreeL m) m a
scalar = labelNestMaybe "scalar" Penny.Ledger.scalar

noScalar
  :: Ledger m
  => Matcher (TreeL m) m ()
noScalar = do
  t <- getSubject
  maySclr <- lift $ Penny.Ledger.scalar t
  case maySclr of
    Nothing -> proclaim "tree has no scalar" >> accept ()
    Just _ -> proclaim "tree has a scalar" >> reject

realm
  :: Ledger m
  => Matcher Realm m a
  -> Matcher (TreeL m) m a
realm = labelNest "realm" Penny.Ledger.realm


offspring
  :: Ledger m
  => Matcher (Seq (TreeL m)) m a
  -> Matcher (TreeL m) m a
offspring = labelNest "offspring" Penny.Ledger.offspring

-- | Succeeds only if this 'TreeL' has no offspring.
noOffspring
  :: Ledger m
  => Matcher (TreeL m) m ()
noOffspring = Penny.Ledger.Matcher.offspring $ do
  sq <- getSubject
  if Seq.null sq
    then proclaim "tree has offspring" >> accept ()
    else proclaim "tree has no offspring" >> reject

-- # Trees

-- | Traverses this tree and all child trees, in pre-order; that is,
-- the node is visited, followed by visiting all its child nodes.
preOrder
  :: Ledger m
  => Matcher (TreeL m) m a
  -> Matcher (TreeL m) m a
preOrder mtcr = do
  s <- getSubject
  cs <- lift $ Penny.Ledger.offspring s
  (inform "pre-order search - this node" >> indent mtcr) <|>
    (inform "pre-order search - children" >>
      (F.asum . fmap (indent . study (preOrder mtcr)) $ cs))


-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: Ledger m
  => Matcher (TreeL m) m a
  -> Matcher (TreeL m) m a
postOrder mtcr = do
  s <- getSubject
  cs <- lift $ Penny.Ledger.offspring s
  (inform "post-order search - children" >>
    (F.asum . fmap (study (postOrder mtcr)) $ cs))
  <|> (inform "post-order search - this node" >> mtcr)


-- # Transactions

txnMeta
  :: Ledger m
  => Matcher (Seq (TreeL m)) m a
  -> Matcher (TransactionL m) m a
txnMeta = labelNest "txnMeta" Penny.Ledger.txnMeta

topLineSer
  :: Ledger m
  => Matcher TopLineSer m a
  -> Matcher (TransactionL m) m a
topLineSer = labelNest "topLineSer" Penny.Ledger.topLineSer


postings
  :: Ledger m
  => Matcher (Seq (PostingL m)) m a
  -> Matcher (TransactionL m) m a
postings = labelNest "postings" Penny.Ledger.postings


-- # Postings

pstgMeta
  :: Ledger m
  => Matcher (Seq (TreeL m)) m a
  -> Matcher (PostingL m) m a
pstgMeta = labelNest "pstgMeta" Penny.Ledger.pstgMeta


trio
  :: Ledger m
  => Matcher Trio m a
  -> Matcher (PostingL m) m a
trio = labelNest "trio" Penny.Ledger.trio


qty
  :: Ledger m
  => Matcher Qty m a
  -> Matcher (PostingL m) m a
qty = labelNest "qty" Penny.Ledger.qty


commodity
  :: Ledger m
  => Matcher Commodity m a
  -> Matcher (PostingL m) m a
commodity = labelNest "commodity" Penny.Ledger.commodity


postingSer
  :: Ledger m
  => Matcher PostingSer m a
  -> Matcher (PostingL m) m a
postingSer = labelNest "postingSer" Penny.Ledger.postingSer

{-# LANGUAGE OverloadedStrings #-}

-- | Pattern matching.
module Penny.Queries.Pattern where

import qualified Data.Foldable as F
import Control.Applicative
import Penny.Lincoln (Matcher)
import qualified Penny.Lincoln as L hiding (fromTo)
import qualified Penny.Ledger as L
import Data.Sequence (Seq)
import Data.Monoid

labelNest
  :: Monad m
  => L.Opinion
  -> (t -> m t')
  -> Matcher t' m a
  -> Matcher t  m a
labelNest op get mr = do
  L.inform ("nesting: " <> op)
  L.nest get mr

dateTime
  :: L.Ledger m
  => Matcher L.DateTime m a
  -> Matcher (L.PriceL m) m a
dateTime = labelNest "dateTime" L.dateTime

fromTo
  :: L.Ledger m
  => Matcher L.FromTo m a
  -> Matcher (L.PriceL m) m a
fromTo = labelNest "fromTo" L.fromTo

exchange
  :: L.Ledger m
  => Matcher L.Exch m a
  -> Matcher (L.PriceL m) m a
exchange = labelNest "exchange" L.exchange

scalar
  :: L.Ledger m
  => Matcher (Maybe L.Scalar) m a
  -> Matcher (L.TreeL m) m a
scalar = labelNest "scalar" L.scalar

realm
  :: L.Ledger m
  => Matcher L.Realm m a
  -> Matcher (L.TreeL m) m a
realm = labelNest "realm" L.realm


offspring
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.TreeL m) m a
offspring = labelNest "offspring" L.offspring


txnMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.TransactionL m) m a
txnMeta = labelNest "txnMeta" L.txnMeta

topLineSer
  :: L.Ledger m
  => Matcher L.TopLineSer m a
  -> Matcher (L.TransactionL m) m a
topLineSer = labelNest "topLineSer" L.topLineSer


postings
  :: L.Ledger m
  => Matcher (Seq (L.PostingL m)) m a
  -> Matcher (L.TransactionL m) m a
postings = labelNest "postings" L.postings


pstgMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.PostingL m) m a
pstgMeta = labelNest "pstgMeta" L.pstgMeta


trio
  :: L.Ledger m
  => Matcher L.Trio m a
  -> Matcher (L.PostingL m) m a
trio = labelNest "trio" L.trio


qty
  :: L.Ledger m
  => Matcher L.Qty m a
  -> Matcher (L.PostingL m) m a
qty = labelNest "qty" L.qty


commodity
  :: L.Ledger m
  => Matcher L.Commodity m a
  -> Matcher (L.PostingL m) m a
commodity = labelNest "commodity" L.commodity


postingSer
  :: L.Ledger m
  => Matcher L.PostingSer m a
  -> Matcher (L.PostingL m) m a
postingSer = labelNest "postingSer" L.postingSer


-- | Traverses this tree and all child trees, in pre-order; that is,
-- the node is visited, followed by visiting all its child nodes.
preOrder
  :: L.Ledger m
  => Matcher (L.TreeL m) m a
  -> Matcher (L.TreeL m) m a
preOrder mtcr = do
  s <- L.getSubject
  cs <- L.offspring s
  (L.inform "pre-order search - this node" >> mtcr) <|>
    (L.inform "pre-order search - children" >>
      (F.asum . fmap (L.localSubject (preOrder mtcr)) $ cs))


-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: L.Ledger m
  => Matcher (L.TreeL m) m a
  -> Matcher (L.TreeL m) m a
postOrder mtcr = do
  s <- L.getSubject
  cs <- L.offspring s
  (L.inform "post-order search - children" >>
    (F.asum . fmap (L.localSubject (postOrder mtcr)) $ cs))
  <|> (L.inform "post-order search - this node" >> mtcr)


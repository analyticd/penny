{-# LANGUAGE OverloadedStrings #-}

-- | Pattern matching.
module Penny.Queries.Pattern where

import qualified Data.Foldable as F
import Pipes
import Control.Applicative
import Penny.Lincoln (Matcher)
import qualified Penny.Lincoln as L
import Data.Sequence (Seq)

instant
  :: L.Ledger m
  => Matcher L.DateTime m a
  -> Matcher (L.PriceL m) m a
instant = L.nestL "instant" L.instant

trade
  :: L.Ledger m
  => Matcher L.FromTo m a
  -> Matcher (L.PriceL m) m a
trade = L.nestL "trade" L.trade

exchange
  :: L.Ledger m
  => Matcher L.Exch m a
  -> Matcher (L.PriceL m) m a
exchange = L.nestL "exchange" L.exchange

capsule
  :: L.Ledger m
  => Matcher (Maybe L.Scalar) m a
  -> Matcher (L.TreeL m) m a
capsule = L.nestL "capsule" L.capsule

namespace
  :: L.Ledger m
  => Matcher L.Realm m a
  -> Matcher (L.TreeL m) m a
namespace = L.nestL "namespace" L.namespace


offspring
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.TreeL m) m a
offspring = L.nestL "offspring" L.offspring


txnMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.TransactionL m) m a
txnMeta = L.nestL "txnMeta" L.txnMeta

zonk
  :: L.Ledger m
  => Matcher L.TopLineSer m a
  -> Matcher (L.TransactionL m) m a
zonk = L.nestL "zonk" L.zonk


plinks
  :: L.Ledger m
  => Matcher (Seq (L.PostingL m)) m a
  -> Matcher (L.TransactionL m) m a
plinks = L.nestL "plinks" L.plinks


plinkMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.PostingL m) m a
plinkMeta = L.nestL "plinkMeta" L.plinkMeta


triplet
  :: L.Ledger m
  => Matcher L.Trio m a
  -> Matcher (L.PostingL m) m a
triplet = L.nestL "triplet" L.triplet


quant
  :: L.Ledger m
  => Matcher L.Qty m a
  -> Matcher (L.PostingL m) m a
quant = L.nestL "quant" L.quant


curren
  :: L.Ledger m
  => Matcher L.Commodity m a
  -> Matcher (L.PostingL m) m a
curren = L.nestL "curren" L.curren


xylo
  :: L.Ledger m
  => Matcher L.PostingSer m a
  -> Matcher (L.PostingL m) m a
xylo = L.nestL "xylo" L.xylo


-- | Traverses this tree and all child trees, in pre-order; that is,
-- the node is visited, followed by visiting all its child nodes.
preOrder
  :: L.Ledger m
  => Matcher (L.TreeL m) m a
  -> Matcher (L.TreeL m) m a
preOrder mtcr = do
  s <- L.getSubject
  cs <- L.offspring s
  L.labeled "pre-order search - this node" mtcr <|>
    L.labeled "pre-order search - children"
      (F.asum . fmap (L.localSubject (preOrder mtcr)) $ cs)


-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: L.Ledger m
  => Matcher (L.TreeL m) m a
  -> Matcher (L.TreeL m) m a
postOrder mtcr = do
  s <- L.getSubject
  cs <- L.offspring s
  L.labeled "post-order search - children"
    (F.asum . fmap (L.localSubject (postOrder mtcr)) $ cs)
  <|> L.labeled "post-order search - this node" mtcr


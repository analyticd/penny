{-# LANGUAGE OverloadedStrings #-}

-- | Pattern matching.
module Penny.Queries.Pattern where

import qualified Data.Foldable as F
import Control.Applicative
import Penny.Lincoln (Matcher)
import qualified Penny.Lincoln as L
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

trade
  :: L.Ledger m
  => Matcher L.FromTo m a
  -> Matcher (L.PriceL m) m a
trade = labelNest "trade" L.trade

exchange
  :: L.Ledger m
  => Matcher L.Exch m a
  -> Matcher (L.PriceL m) m a
exchange = labelNest "exchange" L.exchange

capsule
  :: L.Ledger m
  => Matcher (Maybe L.Scalar) m a
  -> Matcher (L.TreeL m) m a
capsule = labelNest "capsule" L.capsule

namespace
  :: L.Ledger m
  => Matcher L.Realm m a
  -> Matcher (L.TreeL m) m a
namespace = labelNest "namespace" L.namespace


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

zonk
  :: L.Ledger m
  => Matcher L.TopLineSer m a
  -> Matcher (L.TransactionL m) m a
zonk = labelNest "zonk" L.zonk


plinks
  :: L.Ledger m
  => Matcher (Seq (L.PostingL m)) m a
  -> Matcher (L.TransactionL m) m a
plinks = labelNest "plinks" L.plinks


plinkMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.PostingL m) m a
plinkMeta = labelNest "plinkMeta" L.plinkMeta


triplet
  :: L.Ledger m
  => Matcher L.Trio m a
  -> Matcher (L.PostingL m) m a
triplet = labelNest "triplet" L.triplet


quant
  :: L.Ledger m
  => Matcher L.Qty m a
  -> Matcher (L.PostingL m) m a
quant = labelNest "quant" L.quant


curren
  :: L.Ledger m
  => Matcher L.Commodity m a
  -> Matcher (L.PostingL m) m a
curren = labelNest "curren" L.curren


xylo
  :: L.Ledger m
  => Matcher L.PostingSer m a
  -> Matcher (L.PostingL m) m a
xylo = labelNest "xylo" L.xylo


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


-- | TODO make the types more general; they generally do not need a clatch
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
module Penny.Clatch.Shortcut where

import Control.Lens
import Control.Monad
import Data.Monoid
import Penny.Matcher
import Penny.Ledger (Ledger, TreeL, PostingL)
import qualified Penny.Ledger
import Penny.Ledger.Matcher
import qualified Penny.Ledger as L
import Penny.Clatch
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.SeqUtil
import Penny.Transbox
import Penny.Viewpost
import qualified Data.Traversable as Tr
import Data.Text (Text)
import qualified Data.Foldable as F
import Penny.Field.Matcher
import Penny.ListT

standard
  :: Ledger m
  => Text
  -> (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
standard nm getter
  = namedTree nm
  <=< return . join
  <=< Tr.mapM Penny.Ledger.pstgMeta
  . getter
  . view (transboxee.viewpost)

posting
  :: ((View a -> Seq a) -> t)
  -> t
posting fn = fn (Seq.singleton . view onView)


siblings
  :: ((View a -> Seq a) -> t)
  -> t
siblings fn = fn (\vw -> vw ^. onLeft <> vw ^. onRight)

-- | Returns the child trees of standard payee, or the tree itself for
-- shortcut payee.
payee
  :: Ledger m
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
payee getter clch = standard "payee" getter clch `mplus` shortcut
  where
    shortcut
      = F.msum . fmap (\tr -> shortcutMtcr tr >> return (Seq.singleton tr))
      <=< Penny.Ledger.txnMeta . view transaction
      $ clch
    shortcutMtcr tree = do
      guard . Seq.null <=< L.offspring $ tree
      txt <- text <=< just <=< Penny.Ledger.scalar $ tree
      guard . (/= '(') <=< just . (^? _head) $ txt
      guard . (/= ')') <=< just . (^? _last) $ txt

account
  :: Ledger m
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
account getter clch = standard "account" getter clch `mplus` shortcut
  where
    shortcut = F.msum
      . fmap (\tr -> shortcutMtcr tr >> Penny.Ledger.offspring tr)
      <=< return . join
      <=< Tr.mapM Penny.Ledger.pstgMeta . getter . view (transboxee.viewpost)
      $ clch
    shortcutMtcr tree = do
      guard . not . Seq.null <=< L.offspring $ tree
      nothing <=< Penny.Ledger.scalar $ tree

tags
  :: Ledger m
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
tags getter clch = standard "tags" getter clch `mplus` shortcut
  where
    shortcut
      = F.msum . fmap (\tr -> shortcutMtcr tr >> Penny.Ledger.offspring tr)
      <=< Penny.Ledger.txnMeta . view transaction
      $ clch
    shortcutMtcr tree = do
      nothing <=< Penny.Ledger.scalar $ tree
      guard . not . Seq.null <=< L.offspring $ tree

flag
  :: Ledger m
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
flag getter clch = standard "flag" getter clch
  `mplus` shortPstg
  `mplus` shortTop
  where
    shortTop
      = F.msum . fmap (\tr -> shortcutMtcr tr >> return (Seq.singleton tr))
      <=< Penny.Ledger.txnMeta . view transaction
      $ clch
    shortPstg
      = F.msum . fmap (\tr -> shortcutMtcr tr >> return (Seq.singleton tr))
      <=< return . join
      <=< Tr.mapM Penny.Ledger.pstgMeta . getter . view (transboxee.viewpost)
      $ clch
    shortcutMtcr tree = do
      txt <- text <=< just <=< Penny.Ledger.scalar $ tree
      guard . (== '(') <=< just . (^? _head) $ txt
      guard . (== ')') <=< just . (^? _last) $ txt


number
  :: Ledger m
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
number getter clch = standard "number" getter clch
  `mplus` shortPstg
  `mplus` shortTop
  where
    shortTop
      = F.msum . fmap (\tr -> shortcutMtcr tr >> return (Seq.singleton tr))
      <=< Penny.Ledger.txnMeta . view transaction
      $ clch
    shortPstg
      = F.msum . fmap (\tr -> shortcutMtcr tr >> return (Seq.singleton tr))
      <=< return . join
      <=< Tr.mapM Penny.Ledger.pstgMeta . getter . view (transboxee.viewpost)
      $ clch
    shortcutMtcr tree = do
      integer <=< just <=< Penny.Ledger.scalar $ tree
      guard . not . Seq.null <=< Penny.Ledger.offspring $ tree

date
  :: Ledger m
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
date getter clch = standard "date" getter clch `mplus` shortTop
  where
    shortTop
      = F.msum . fmap (\tr -> shortcutMtcr tr >> return (Seq.singleton tr))
      <=< Penny.Ledger.txnMeta . view transaction
      $ clch
    shortcutMtcr tree = do
      Penny.Field.Matcher.date <=< just <=< Penny.Ledger.scalar $ tree
      guard . not . Seq.null <=< Penny.Ledger.offspring $ tree

time
  :: Ledger m
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
time getter clch = standard "time" getter clch `mplus` shortTop
  where
    shortTop
      = F.msum . fmap (\tr -> shortcutMtcr tr >> return (Seq.singleton tr))
      <=< Penny.Ledger.txnMeta . view transaction
      $ clch
    shortcutMtcr tree = do
      Penny.Field.Matcher.time <=< just <=< Penny.Ledger.scalar $ tree
      guard . not . Seq.null <=< Penny.Ledger.offspring $ tree


zone
  :: Ledger m
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> ListT m (Seq (TreeL m))
zone getter clch = standard "zone" getter clch `mplus` shortTop
  where
    shortTop
      = F.msum . fmap (\tr -> shortcutMtcr tr >> return (Seq.singleton tr))
      <=< Penny.Ledger.txnMeta . view transaction
      $ clch
    shortcutMtcr tree = do
      Penny.Field.Matcher.zone <=< just <=< Penny.Ledger.scalar $ tree
      guard . not . Seq.null <=< Penny.Ledger.offspring $ tree

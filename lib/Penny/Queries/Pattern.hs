{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Patterns to match on trees and forests.
--
-- A tree is the same depth as the forest containing the tree.
--
-- A forest contained in a tree is one level deeper than the tree.
module Penny.Queries.Pattern where

import Pipes
import Control.Applicative
import Control.Monad.Reader
import qualified Penny.Lincoln as L
import Turtle.Pattern
import Data.Text (Text)

newtype Matcher t m a = Matcher (ReaderT t (ListT m) a)
  deriving (Functor, Applicative, Monad)

deriving instance Monad m => MonadReader t (Matcher t m)
deriving instance Monad m => Alternative (Matcher t m)
deriving instance Monad m => MonadPlus (Matcher t m)

component :: Monad m => (t' -> m t) -> Matcher t m a -> Matcher t' m a
component conv (Matcher (ReaderT f)) = Matcher $ ReaderT $ \r -> do
  r' <- lift (conv r)
  f r'

realm
  :: Monad m
  => (L.Realm -> Bool)
  -> Matcher L.Realm m L.Realm
realm pd = do
  rlm <- ask
  if pd rlm then return rlm else empty

namespace
  :: L.Ledger m
  => Matcher L.Realm m a
  -> Matcher (L.TreeL m) m a
namespace = component L.namespace

pattern :: Monad m => Pattern a -> Matcher Text m a
pattern pat = Matcher . ReaderT $ \txt ->
  let ls = match pat txt
  in Select . each $ ls

{-
  ( -- * Patterns on trees
    TreePat(..)
  , matchTree

  -- ** Levels
  , treeLevel

  -- ** Payload
  , payload
  , noPayload

  -- ** Namespace
  , namespace

  -- ** Child trees
  , children

  -- ** Traversal
  , preOrder
  , postOrder

  -- * Forest patterns
  , ForestPat(..)
  , matchForest
  , forestLevel
  , anyTree
  , allTrees
  , oneTree
  ) where


import qualified Penny.Lincoln as L
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as S
import qualified Penny.Queries.Matcher as M

-- # Tree patterns

newtype TreePat m a = TreePat (ReaderT (L.Unsigned, L.TreeL m) (MaybeT m) a)
  deriving (Functor, Monad, Applicative, Alternative, MonadPlus)

matchTree
  :: Monad m
  => L.Unsigned
  -- ^ Start at this level; ordinarily you will use 'L.toUnsigned'
  -- 'L.Zero'

  -> L.TreeL m
  -> TreePat m a
  -> m (Maybe a)
matchTree l t (TreePat p)
  = runMaybeT . runReaderT p $ (l, t)

tree :: Monad l => TreePat l (L.TreeL l)
tree = TreePat $ asks snd


treeLevel :: Monad l => TreePat l L.Unsigned
treeLevel = TreePat $ asks fst

inLedger :: Monad l => l a -> TreePat l a
inLedger = TreePat . lift . lift

payload
  :: L.Ledger l
  => M.Matcher L.Scalar l r
  -> TreePat l r
payload mr = do
  tr <- tree
  maySc <- inLedger . L.scalar $ tr
  case maySc of
    Nothing -> mzero
    Just sc -> do
      mayR <- inLedger $ M.runMatcher mr sc
      maybe mzero return mayR

noPayload :: L.Ledger l => TreePat l ()
noPayload = do
  tr <- tree
  sc <- inLedger . L.scalar $ tr
  maybe (return ()) (const mzero) sc

namespace :: L.Ledger l => M.Matcher L.Realm l a -> TreePat l a
namespace mr = do
  tr <- tree
  nms <- inLedger $ L.realm tr
  mayR <- inLedger $ M.runMatcher mr nms
  maybe mzero return mayR

children :: L.Ledger l => ForestPat l a -> TreePat l a
children (ForestPat fp) = TreePat $ do
  (thisLevel, tr) <- ask
  let nextLevel = L.next thisLevel
  cs <- lift . lift . L.children $ tr
  mayRes <- lift . lift . runMaybeT . runReaderT fp
    $ (nextLevel, cs)
  maybe mzero return mayRes

-- # Traversal

preOrder :: L.Ledger l => TreePat l a -> TreePat l a
preOrder p@(TreePat tp) = TreePat $ do
  (l, t) <- ask
  mayR <- lift . lift . runMaybeT . runReaderT tp $ (l, t)
  case mayR of
    Just r -> return r
    Nothing -> do
      ts <- lift . lift . L.children $ t
      let go sq = case S.viewl sq of
            EmptyL -> mzero
            x :< xs -> do
              let TreePat k = preOrder p
              mayR' <- lift . lift . runMaybeT . runReaderT k
                $ (L.next l, x)
              maybe (go xs) return mayR'
      go ts


-- There is no in-order traversal for trees.  There's no way to
-- (sensibly) traverse a rose tree in a symmetric fashion.

postOrder :: L.Ledger l => TreePat l a -> TreePat l a
postOrder  p@(TreePat tp) = TreePat $ do
  (l, t) <- ask
  let go sq = case S.viewl sq of
        EmptyL -> do
          mayR <- lift . lift . runMaybeT . runReaderT tp $ (l, t)
          maybe mzero return mayR
        x :< xs -> do
          let TreePat k = postOrder p
          mayR <- lift . lift . runMaybeT . runReaderT k
            $ (L.next l, x)
          maybe (go xs) return mayR
  ts <- lift . lift . L.children $ t
  go ts

-- # Forest patterns

newtype ForestPat m a
  = ForestPat (ReaderT (L.Unsigned, Seq (L.TreeL m)) (MaybeT m) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

matchForest
  :: Monad m
  => L.Unsigned
  -- ^ Start at this level; ordinarily you will use 'L.toUnsigned'
  -- 'L.Zero'

  -> Seq (L.TreeL m)
  -> ForestPat m a
  -> m (Maybe a)
matchForest l fr (ForestPat p)
  = runMaybeT . runReaderT p $ (l, fr)

forestLevel :: Monad l => ForestPat l L.Unsigned
forestLevel = ForestPat $ asks fst


anyTree :: Monad m => TreePat m a -> ForestPat m a
anyTree (TreePat tp) = ForestPat $ do
  (lvlU, trs) <- ask
  let go ts = case S.viewl ts of
        EmptyL -> return Nothing
        x :< xs -> do
          mayR <- runMaybeT . runReaderT tp $ (lvlU, x)
          maybe (go xs) (return . Just) mayR
  res <- lift . lift $ go trs
  maybe mzero return res

allTrees :: Monad m => TreePat m a -> ForestPat m (Seq a)
allTrees (TreePat tp) = ForestPat $ do
  (lvlU, trs) <- ask
  let go ts acc = case S.viewl ts of
        EmptyL -> return . Just $ acc
        x :< xs -> do
          mayR <- runMaybeT . runReaderT tp $ (lvlU, x)
          maybe (return Nothing) (\r -> go xs (acc |> r)) mayR
  res <- lift . lift $ go trs S.empty
  maybe mzero return res

oneTree :: Monad m => Int -> TreePat m a -> ForestPat m a
oneTree idx (TreePat tp) = ForestPat $ do
  (lvlU, trs) <- ask
  guard (S.length trs > idx)
  let chld = S.index trs idx
  mayR <- lift . lift . runMaybeT . runReaderT tp $ (lvlU, chld)
  maybe mzero return mayR
-}

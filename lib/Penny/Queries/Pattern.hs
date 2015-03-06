{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.Queries.Pattern where

import qualified Penny.Lincoln as L
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad
import Data.Sequence (Seq)

-- The tree is the same depth as the forest containing the tree.
--
-- The forest contained in a tree is one level deeper than the tree.

data TreePatEnv l
  = TreePatEnv (L.TreeL l) (L.Unsigned)
  -- ^ @TreePatEnv t u@, where
  --
  -- @t@ is the tree under study, and
  --
  -- @u@ is the depth of this tree from the top.

newtype TreePat m a = TreePat (ReaderT (TreePatEnv m) (MaybeT m) a)
  deriving (Functor, Monad, Applicative, Alternative, MonadPlus)

tree :: Monad l => TreePat l (L.TreeL l)
tree = TreePat $ do
  TreePatEnv t _ <- ask
  return t

treeLevel :: Monad l => TreePat l L.Unsigned
treeLevel = TreePat $ do
  TreePatEnv _ l <- ask
  return l

inLedger :: Monad l => l a -> TreePat l a
inLedger = TreePat . lift . lift

-- | The tree's scalar value.
scalar :: L.Ledger l => TreePat l L.Scalar
scalar = do
  tr <- tree
  sc <- inLedger . L.scalar $ tr
  case sc of
    Nothing -> mzero
    Just s -> return s

realm :: L.Ledger l => TreePat l L.Realm
realm = tree >>= inLedger . L.realm

-- | The tree is in the User realm.
user :: L.Ledger l => TreePat l ()
user = do
  rlm <- realm
  guard (rlm == L.User)

-- | The tree is in the System realm.
system :: L.Ledger l => TreePat l ()
system = do
  rlm <- realm
  guard (rlm == L.System)

children :: L.Ledger l => ForestPat l a -> TreePat l a
children = undefined

data ForestPatEnv l
  = ForestPatEnv (Seq (L.TreeL l)) L.Unsigned
  -- ^ @ForestPatEnv r u@, where
  --
  -- @r@ is the forest under study, and
  --
  -- @u@ is the depth from the top.

newtype ForestPat m a = ForestPat (ReaderT (ForestPatEnv m) (MaybeT m) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

forestLevel :: Monad l => ForestPat l L.Unsigned
forestLevel = ForestPat $ do
  ForestPatEnv _ u <- ask
  return u

trees :: Monad l => ForestPat l (Seq (L.TreeL l))
trees = ForestPat $ do
  ForestPatEnv ts _ <- ask
  return ts

anyTree :: Monad m => TreePat m a -> ForestPat m a
anyTree = undefined

allTrees :: Monad m => TreePat m a -> ForestPat m (Seq a)
allTrees = undefined

index :: Monad m => Int -> TreePat m a -> ForestPat m a
index = undefined

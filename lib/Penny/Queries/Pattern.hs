{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Patterns to match on trees and forests.
--
-- A tree is the same depth as the forest containing the tree.
--
-- A forest contained in a tree is one level deeper than the tree.
module Penny.Queries.Pattern
  ( -- * Patterns on trees
    TreePat(..)
  , matchTree

  -- ** Levels
  , treeLevel

  -- ** Scalars
  , scalar
  , field
  , equals
  , notEquals
  , greaterThan
  , lessThan
  , isPrefixOf
  , isSuffixOf
  , isInfixOf

  -- ** Realm
  , realm
  , user
  , system

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
import Data.Text (Text)
import qualified Data.Text as X

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

scalar :: L.Ledger l => TreePat l L.Scalar
scalar = do
  tr <- tree
  sc <- inLedger . L.scalar $ tr
  maybe mzero return sc

field :: (L.Ledger l, L.Field a) => TreePat l a
field = scalar >>= maybe mzero return . L.fromScalar

equals :: (L.Ledger l, L.Field a, Eq a) => a -> TreePat l ()
equals t = do
  r <- field
  guard (r == t)

notEquals :: (L.Ledger l, L.Field a, Eq a) => a -> TreePat l ()
notEquals t = do
  r <- field
  guard (r /= t)

greaterThan :: (L.Ledger l, L.Field a, Ord a) => a -> TreePat l ()
greaterThan t = do
  r <- field
  guard (r > t)

lessThan :: (L.Ledger l, L.Field a, Ord a) => a -> TreePat l ()
lessThan t = do
  r <- field
  guard (r < t)

isPrefixOf :: L.Ledger l => Text -> TreePat l ()
isPrefixOf pfx = do
  r <- field
  guard (pfx `X.isPrefixOf` r)

isSuffixOf :: L.Ledger l => Text -> TreePat l ()
isSuffixOf sfx = do
  r <- field
  guard (sfx `X.isSuffixOf` r)

isInfixOf :: L.Ledger l => Text -> TreePat l ()
isInfixOf ifx = do
  r <- field
  guard (ifx `X.isInfixOf` r)

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

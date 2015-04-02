{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Patterns to match on trees and forests.
--
-- A tree is the same depth as the forest containing the tree.
--
-- A forest contained in a tree is one level deeper than the tree.
module Penny.Queries.Pattern where

import qualified Data.Foldable as F
import Control.Monad.Logic
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Penny.Lincoln as L
import Turtle.Pattern
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

--newtype Matcher t m a = Matcher (ReaderT t (LogicT m) a)
--  deriving (Functor, Applicative, Monad)

newtype Level = Level L.Unsigned
  deriving (Eq, Ord, Show)

newtype Opinion = Opinion Text
  deriving (Eq, Ord, Show)

data Verdict
  = Matched
  | Rejected
  deriving (Eq, Ord, Show)

data Message = Message Level (Maybe Verdict) Opinion
  deriving (Eq, Ord, Show)

newtype Matcher t m a
  = Matcher (WriterT (Seq Message) (ReaderT (t, Level) (LogicT m)) a)
  deriving (Functor, Applicative, Monad)

target :: Monad m => Matcher t m t
target = Matcher . WriterT . ReaderT $ \(t, _) -> return (t, Seq.empty)

instance Monad m => Alternative (Matcher t m) where
  empty = Matcher . WriterT . ReaderT $ \_ -> mzero
  (Matcher (WriterT (ReaderT fx))) <|> (Matcher (WriterT (ReaderT fy)))
    = Matcher . WriterT . ReaderT $ \env ->
    (fx env) <|> (fy env)

instance MonadIO m => MonadIO (Matcher t m) where
  liftIO act = Matcher . WriterT . ReaderT $ \_ ->
    fmap (\r -> (r, Seq.empty)) (liftIO act)

instance MonadTrans (Matcher t) where
  lift act = Matcher . WriterT . ReaderT $ \_ ->
    fmap (\r -> (r, Seq.empty)) (lift act)

instance Monad m => MonadPlus (Matcher t m) where
  mzero = Matcher . WriterT . ReaderT $ \_ -> mzero
  mplus (Matcher (WriterT (ReaderT getX))) (Matcher (WriterT (ReaderT getY)))
    = Matcher . WriterT . ReaderT $ \env ->
    mplus (getX env) (getY env)

{-
instance Monad m => MonadLogic (Matcher t m) where
  -- msplit :: Matcher t m a -> Matcher t m (Maybe (a, Matcher t m a))
  msplit (Matcher (WriterT (ReaderT getAct)))
    = Matcher . WriterT . ReaderT $ \env ->
    let act = getAct env
        -- splitRes :: LogicT m (Maybe (a, LogicT m a))
        splitRes = msplit act
    in fmap (fmap ((\(a, lt) -> (a, Matcher . WriterT . ReaderT $ \_ -> lt))))
            splitRes
-}
{-

applyMatcher :: Matcher t m a -> t -> LogicT m a
applyMatcher (Matcher (ReaderT f)) env = f env

rewrapMatcher :: (t -> LogicT m a) -> Matcher t m a
rewrapMatcher f = Matcher . ReaderT $ f


instance Monad m => MonadLogic (Matcher t m) where
  -- msplit :: Matcher t m a -> Matcher t m (Maybe (a, Matcher t m a))
  msplit (Matcher (ReaderT getAct)) = Matcher . ReaderT $ \env ->
    let act = getAct env
        -- splitRes :: LogicT m (Maybe (a, LogicT m a))
        splitRes = msplit act
    in fmap (fmap ((\(a, lt) -> (a, Matcher . ReaderT $ \_ -> lt))))
            splitRes
instance L.Ledger m => L.Ledger (Matcher t m) where
  type PriceL (Matcher t m) = L.PriceL m
  type TransactionL (Matcher t m) = L.TransactionL m
  type TreeL (Matcher t m) = L.TreeL m
  type PostingL (Matcher t m) = L.PostingL m

  vault = lift L.vault
  instant = lift . L.instant
  trade = lift . L.trade
  exchange = lift . L.exchange
  capsule = lift . L.capsule
  namespace = lift . L.namespace
  offspring = lift . L.offspring
  txnMeta = lift . L.txnMeta
  zonk = lift . L.zonk
  plinks = lift . L.plinks
  plinkMeta = lift . L.plinkMeta
  triplet = lift . L.triplet
  quant = lift . L.quant
  curren = lift . L.curren
  xylo = lift . L.xylo

component :: Monad m => (t' -> m t) -> Matcher t m a -> Matcher t' m a
component conv (Matcher (ReaderT f)) = Matcher $ ReaderT $ \r ->
  lift (conv r) >>= f

-- | @predicate f@ creates a 'Matcher' that will match any value for
-- which @f@ returns True.  The value itself is returned as a witness.
predicate
  :: Monad m
  => (a -> Bool)
  -> Matcher a m a
predicate pd = rewrapMatcher f
  where
    f t
      | pd t = return t
      | otherwise = empty

pattern :: Monad m => Pattern a -> Matcher Text m a
pattern pat = do
  txt <- ask
  foldr interleave empty . map return . match pat $ txt

just :: Monad m => Matcher t m a -> Matcher (Maybe t) m a
just pd = rewrapMatcher f
  where
    f may = case may of
      Nothing -> empty
      Just t -> applyMatcher pd t


nothing :: Monad m => Matcher (Maybe a) m ()
nothing = do
  tgt <- ask
  case tgt of
    Nothing -> return ()
    _ -> empty

each
  :: (Monad m, Functor f, F.Foldable f)
  => Matcher t m a
  -> Matcher (f t) m a
each (Matcher (ReaderT getAct)) = Matcher . ReaderT $
  F.foldl interleave empty . fmap getAct


index
  :: Int
  -> Matcher t m a
  -> Matcher (Seq t) m a
index idx mtchr = rewrapMatcher f
  where
    f sq
      | idx >= 0 && idx < Seq.length sq = applyMatcher mtchr
          (sq `Seq.index` idx)
      | otherwise = empty


instant
  :: L.Ledger m
  => Matcher L.DateTime m a
  -> Matcher (L.PriceL m) m a
instant = component L.instant

trade
  :: L.Ledger m
  => Matcher L.FromTo m a
  -> Matcher (L.PriceL m) m a
trade = component L.trade

exchange
  :: L.Ledger m
  => Matcher L.Exch m a
  -> Matcher (L.PriceL m) m a
exchange = component L.exchange

capsule
  :: L.Ledger m
  => Matcher (Maybe L.Scalar) m a
  -> Matcher (L.TreeL m) m a
capsule = component L.capsule

namespace
  :: L.Ledger m
  => Matcher L.Realm m a
  -> Matcher (L.TreeL m) m a
namespace = component L.namespace


offspring
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.TreeL m) m a
offspring = component L.offspring


txnMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.TransactionL m) m a
txnMeta = component L.txnMeta

zonk
  :: L.Ledger m
  => Matcher L.TopLineSer m a
  -> Matcher (L.TransactionL m) m a
zonk = component L.zonk


plinks
  :: L.Ledger m
  => Matcher (Seq (L.PostingL m)) m a
  -> Matcher (L.TransactionL m) m a
plinks = component L.plinks


plinkMeta
  :: L.Ledger m
  => Matcher (Seq (L.TreeL m)) m a
  -> Matcher (L.PostingL m) m a
plinkMeta = component L.plinkMeta


triplet
  :: L.Ledger m
  => Matcher L.Trio m a
  -> Matcher (L.PostingL m) m a
triplet = component L.triplet


quant
  :: L.Ledger m
  => Matcher L.Qty m a
  -> Matcher (L.PostingL m) m a
quant = component L.quant


curren
  :: L.Ledger m
  => Matcher L.Commodity m a
  -> Matcher (L.PostingL m) m a
curren = component L.curren


xylo
  :: L.Ledger m
  => Matcher L.PostingSer m a
  -> Matcher (L.PostingL m) m a
xylo = component L.xylo

-- | Traverses this tree and all child trees, in pre-order; that is,
-- the node is visited, followed by visiting all its child nodes.
preOrder
  :: L.Ledger m
  => Matcher (L.TreeL m) m a
  -> Matcher (L.TreeL m) m a
preOrder mtcr = Matcher . ReaderT $ \tr -> do
  cs <- lift $ L.offspring tr
  let acts = fmap (applyMatcher (preOrder mtcr)) cs
      logicThis = applyMatcher mtcr tr
  interleave logicThis . F.foldl interleave empty $ acts


-- | Traverses this tree and all child trees, in post-order; that is,
-- all child nodes are visited, followed by this node.
postOrder
  :: L.Ledger m
  => Matcher (L.TreeL m) m a
  -> Matcher (L.TreeL m) m a
postOrder mtcr = Matcher . ReaderT $ \tr -> do
  cs <- lift $ L.offspring tr
  let acts = fmap (applyMatcher (postOrder mtcr)) cs
      logicThis = applyMatcher mtcr tr
  interleave (F.foldl interleave empty acts) logicThis
-}

{-# LANGUAGE OverloadedStrings #-}
module Penny.Queries.Pred
  ( -- * Realm
    user
  , system

  -- * Text
  , isPrefixOf
  , isSuffixOf
  , isInfixOf

  -- * Scalar
  , scalar

  -- * Commodity
  , commodity

  -- * Semantics
  , semanticEqual
  , semanticNotEqual
  , semanticGreaterBy
  , semanticLessBy

  -- * Serials
  , serial
  , forward
  , reverse
  , recto
  , verso

  -- * Sequences
  , index

  -- * Trees
  , matchTree
  , levelIs
  , toReader
  , level
  , payload
  , noPayload
  , namespace
  , children
  , postOrder
  ) where

import Prednote (PredM, (&&&))
import qualified Prednote as P
import qualified Penny.Lincoln as L
import Control.Applicative
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid
import Data.Sequence (Seq, ViewR(..))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except
import Rainbow (fromText)
import Data.String
import qualified Data.Traversable as Tr
import Prelude hiding (reverse)

-- # Trees and tree components

user :: Applicative f => PredM f L.Realm
user = P.equalByM "User" (\rlm -> pure (rlm == L.User))

system :: Applicative f => PredM f L.Realm
system = P.equalByM "System" (\rlm -> pure (rlm == L.System))

scalar
  :: (Monad m, Applicative m, L.Field a)
  => PredM m a -> PredM m L.Scalar
scalar p = contramap L.fromScalar $ P.maybe False p

isPrefixOf
  :: (Monad m, Applicative m)
  => Text
  -> PredM m Text
isPrefixOf pfx = P.predicateM $ \txt ->
  pure ( pfx `X.isPrefixOf` txt, P.Value [fromText txt],
         P.Condition [fromText $ "has a prefix of " <> pfx])

isSuffixOf
  :: (Monad m, Applicative m)
  => Text
  -> PredM m Text
isSuffixOf sfx = P.predicateM $ \txt ->
  pure ( sfx `X.isSuffixOf` txt, P.Value [fromText txt],
         P.Condition [fromText $ "has a suffix of " <> sfx])

isInfixOf
  :: (Monad m, Applicative m)
  => Text
  -> PredM m Text
isInfixOf ifx = P.predicateM $ \txt ->
  pure ( ifx `X.isInfixOf` txt, P.Value [fromText txt],
         P.Condition [fromText $ "has a infix of " <> ifx])

commodity
  :: (Monad m, Applicative m)
  => PredM m Text
  -> PredM m L.Commodity
commodity = contramap (\(L.Commodity x) -> x)

semanticEqual
  :: (Applicative m, L.SemanticEq a, Show a)
  => a
  -> PredM m a
semanticEqual a = P.equalByM (X.pack . show $ a)
  (\t -> pure (L.semanticEq a t))

semanticNotEqual
  :: (Applicative m, L.SemanticEq a, Show a)
  => a
  -> PredM m a
semanticNotEqual a = P.notEqByM (X.pack . show $ a)
  (\t -> pure (not $ L.semanticEq a t))

semanticGreaterBy
  :: (Applicative m, L.SemanticOrd a, Show a)
  => a
  -> PredM m a
semanticGreaterBy a = P.greaterByM (X.pack . show $ a)
  (\t -> pure (L.semanticOrd t a))

semanticLessBy
  :: (Applicative m, L.SemanticOrd a, Show a)
  => a
  -> PredM m a
semanticLessBy a = P.lessByM (X.pack . show $ a)
  (\t -> pure (L.semanticOrd t a))

serial :: PredM m L.Unsigned -> PredM m L.Serial
serial = contramap (\(L.Serial s) -> s)

forward :: PredM m L.Serial -> PredM m L.Forward
forward = contramap (\(L.Forward s) -> s)

reverse :: PredM m L.Serial -> PredM m L.Reverse
reverse = contramap (\(L.Reverse s) -> s)

recto :: PredM m L.Forward -> PredM m L.Serset
recto = contramap (\(L.Serset fwd _) -> fwd)

verso :: PredM m L.Reverse -> PredM m L.Serset
verso = contramap (\(L.Serset _ rev) -> rev)

index
  :: (Show a, Monad m, Applicative m)
  => Int -> PredM m a -> PredM m (Seq a)
index idx pd
  = P.addLabel lbl
  $ testLength &&& testVal
  where
    idxLbl = X.pack . show $ idx
    lbl = [fromText $ "test value at sequence index " <> idxLbl]
    testLength = P.predicateM $ \sq -> pure
      ( S.length sq > idx
      , P.Value ["sequence"]
      , P.Condition [fromText $ "has at least " <> idxLbl <> " elements"]
      )
    testVal = contramap (\sq -> S.index sq idx) pd

--

matchTree
  :: Monad l
  => L.Unsigned
  -> L.TreeL l
  -> PredM (ReaderT L.Unsigned l) (L.TreeL l)
  -> l P.Result
matchTree st tr (P.PredM k) = runReaderT (k tr) st

levelIs
  :: (Monad l, Applicative l)
  => L.Unsigned
  -> PredM (ReaderT L.Unsigned l) (L.TreeL l)
levelIs tgt = P.predicateM f
  where
    f _ = (,,) <$> getBool <*> pure val <*> pure cond
      where
        getBool = do
          lvl <- ask
          return $ lvl == tgt
        val = P.Value ["tree"]
        cond = P.Condition
          [ fromString $ "level is " ++ (show . L.naturalToInteger $ tgt) ]

toReader
  :: Monad m
  => PredM m a
  -> PredM (ReaderT a m) b
toReader (P.PredM k) = P.PredM $ \_ -> do
  env <- ask
  lift $ k env

level
  :: (Monad m, Applicative m)
  => PredM m L.Unsigned
  -> PredM (ReaderT L.Unsigned m) (L.TreeL m)
level = P.addLabel ["tree level"] . toReader

payload
  :: L.Ledger m
  => PredM (ReaderT a m) L.Scalar
  -> PredM (ReaderT a m) (L.TreeL m)
payload
  = P.addLabel ["payload"]
  . P.contramapM (\a -> lift (L.scalar a))
  . P.maybe False

noPayload :: L.Ledger m => PredM (ReaderT a m) (L.TreeL m)
noPayload
  = P.addLabel ["no payload"]
  . P.contramapM (\a -> lift (L.scalar a))
  $ P.maybe True P.false

namespace
  :: L.Ledger m
  => PredM (ReaderT a m) L.Realm
  -> PredM (ReaderT a m) (L.TreeL m)
namespace
  = P.addLabel ["namespace"]
  . P.contramapM (\a -> lift (L.realm a))

children
  :: L.Ledger m
  => PredM (ReaderT L.Unsigned m) (Seq (L.TreeL m))
  -> PredM (ReaderT L.Unsigned m) (L.TreeL m)
children (P.PredM k) = P.addLabel ["children"] (P.PredM p')
  where
    p' tr = do
      cs <- lift . L.children $ tr
      local (L.next) (k cs)

postOrder
  :: L.Ledger m
  => PredM (ReaderT L.Unsigned m) (L.TreeL m)
  -> PredM (ReaderT L.Unsigned m) (L.TreeL m)
postOrder (P.PredM pd) = P.addLabel ["pre-order search"] (P.PredM p')
  where
    p' tree = do
      lvl <- ask
      let excT = procPostOrder pd tree
      lift $ finishPostOrder excT lvl

finishPostOrder
  :: L.Ledger m
  => ExceptT (P.Labeled P.Passed)
             (WriterT (Seq (P.Labeled P.Failed)) (ReaderT L.Unsigned m))
             (P.Labeled P.Failed)
  -> L.Unsigned
  -> m P.Result
finishPostOrder exc uns = do
  (ei, sq) <- flip runReaderT uns . runWriterT . runExceptT $ exc
  return $ case ei of
    Right failed -> makeFailed sq failed
    Left psd -> makePassed sq psd


makeFailed
  :: Seq (P.Labeled P.Failed)
  -> P.Labeled P.Failed
  -> P.Result
makeFailed fails (P.Labeled lastLbl lastFail) = case S.viewr fails of
  EmptyR -> P.Result (P.Labeled lastLbl (Left lastFail))
  xs :> x ->
    P.Result . P.Labeled [] . Left $ F.foldr f z xs
    where
      z = P.FOr x (P.Labeled lastLbl lastFail)
      f failThis failRest = P.FOr failThis (P.Labeled [] failRest)

makePassed
  :: Seq (P.Labeled P.Failed)
  -> P.Labeled P.Passed
  -> P.Result
makePassed passes (P.Labeled lastLbl lastPass) = case S.viewr passes of
  EmptyR -> P.Result (P.Labeled lastLbl (Right lastPass))
  xs :> x ->
    P.Result . P.Labeled [] . Right $ F.foldr f z xs
    where
      z = P.POr . Right $ (x, P.Labeled lastLbl lastPass)
      f failThis passRest
        = P.POr . Right $ (failThis, (P.Labeled [] passRest))

procPostOrder
  :: L.Ledger m
  => (L.TreeL m -> ReaderT L.Unsigned m P.Result)
  -> L.TreeL m
  -> ExceptT (P.Labeled P.Passed)
             (WriterT (Seq (P.Labeled P.Failed)) (ReaderT L.Unsigned m))
             (P.Labeled P.Failed)
procPostOrder k tr = do
  cs <- lift . lift . lift $ L.children tr
  let nextR tr' = local L.next (k tr')
      procBelow k' tr' = do
        failed <- procPostOrder k' tr'
        lift $ tell (S.singleton failed)
  _ <- Tr.mapM (procBelow nextR) cs
  P.Result (P.Labeled lbl res) <- lift . lift $ k tr
  case res of
    Right psd -> throwE (P.Labeled lbl psd)
    Left fld -> return (P.Labeled lbl fld)

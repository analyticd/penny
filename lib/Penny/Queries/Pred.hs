{-# LANGUAGE OverloadedStrings #-}
module Penny.Queries.Pred where

import Prednote (PredM, (&&&))
import qualified Prednote as P
import qualified Penny.Lincoln as L
import Control.Applicative
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as X
import Data.Monoid
import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as S
import Control.Monad.Trans.Reader

user :: Applicative f => PredM f L.Realm
user = P.equalByM "User" (\rlm -> pure (rlm == L.User))

system :: Applicative f => PredM f L.Realm
system = P.equalByM "System" (\rlm -> pure (rlm == L.System))

scalar
  :: (Monad m, Applicative m, L.Field a)
  => PredM m a -> PredM m L.Scalar
scalar p = contramap L.fromScalar $ P.maybe P.false p

isPrefixOf
  :: (Monad m, Applicative m)
  => Text
  -> PredM m Text
isPrefixOf pfx = P.predicateM ("has a prefix of " <> pfx)
  (\x -> pure $ pfx `X.isPrefixOf` x)

isSuffixOf
  :: (Monad m, Functor m, Applicative m)
  => Text
  -> PredM m Text
isSuffixOf sfx = P.predicateM ("has a suffix of " <> sfx)
  (\x -> pure $ sfx `X.isSuffixOf` x)

isInfixOf
  :: (Monad m, Applicative m)
  => Text
  -> PredM m Text
isInfixOf ifx = P.predicateM ("contains " <> ifx)
  (\x -> pure $ ifx `X.isInfixOf` x)

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
  = P.addLabel ("test value at sequence index " <> (X.pack . show $ idx))
  $ testLength &&& testVal
  where
    testLength = P.predicateM ("has at least " <> (X.pack . show $ idx)
      <> " elements") (\sq -> pure (S.length sq > idx))
    testVal = contramap (\sq -> S.index sq idx) pd

--

type TreePred l = PredM (ReaderT L.Unsigned l) (L.TreeL l)

matchTree
  :: Monad l
  => L.Unsigned
  -> L.TreeL l
  -> TreePred l
  -> l P.Result
matchTree st tr (P.PredM k) = runReaderT (k tr) st

levelIs :: Monad l => L.Unsigned -> PredM (ReaderT L.Unsigned l) (L.TreeL l)
levelIs tgt = P.predicateM "level is equal to" k
  where
    k _ = do
      lvl <- ask
      return $ lvl == tgt

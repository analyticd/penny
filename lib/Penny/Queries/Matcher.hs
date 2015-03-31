{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | DEPRECATED use "Penny.Queries.Pred" instead
module Penny.Queries.Matcher where

import qualified Penny.Lincoln as L
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as X

newtype Matcher s m a = Matcher (ReaderT s (MaybeT m) a)
  deriving (Functor, Monad, Applicative, Alternative, MonadPlus)


mapSubject :: (s' -> s) -> Matcher s m a -> Matcher s' m a
mapSubject f (Matcher k) = Matcher (withReaderT f k)

runMatcher :: Monad m => Matcher s m a -> s -> m (Maybe a)
runMatcher (Matcher k) s = runMaybeT . runReaderT k $ s

subject :: Monad m => Matcher s m s
subject = Matcher $ ask

user :: Monad m => Matcher L.Realm m ()
user = equal L.User >> return ()

system :: Monad m => Matcher L.Realm m ()
system = equal L.System >> return ()

equal :: (Monad m, Eq s) => s -> Matcher s m s
equal p = do
  s <- subject
  guard $ p == s
  return s

greater :: (Monad m, Ord s) => s -> Matcher s m s
greater p = do
  s <- subject
  guard $ s > p
  return s

less :: (Monad m, Ord s) => s -> Matcher s m s
less p = do
  s <- subject
  guard $ s < p
  return s

semanticEqual :: (Monad m, L.SemanticEq s) => s -> Matcher s m s
semanticEqual p = do
  s <- subject
  guard $ L.semanticEq s p
  return s

semanticNotEqual :: (Monad m, L.SemanticEq s) => s -> Matcher s m s
semanticNotEqual p = do
  s <- subject
  guard . not $ L.semanticEq s p
  return s

semanticGreater :: (Monad m, L.SemanticOrd s) => s -> Matcher s m s
semanticGreater p = do
  s <- subject
  guard $ L.semanticOrd s p == GT
  return s

semanticLess :: (Monad m, L.SemanticOrd s) => s -> Matcher s m s
semanticLess p = do
  s <- subject
  guard $ L.semanticOrd s p == LT
  return s

notEqual :: (Monad m, Eq s) => s -> Matcher s m s
notEqual p = do
  s <- subject
  guard $ s /= p
  return s

isPrefixOf :: Monad m => Text -> Matcher Text m Text
isPrefixOf pfx = do
  s <- subject
  guard $ (pfx `X.isPrefixOf` s)
  return s

isSuffixOf :: Monad m => Text -> Matcher Text m Text
isSuffixOf sfx = do
  s <- subject
  guard $ (sfx `X.isSuffixOf` s)
  return s

isInfixOf :: Monad m => Text -> Matcher Text m Text
isInfixOf ifx = do
  s <- subject
  guard $ (ifx `X.isInfixOf` s)
  return s

scalar :: (Monad m, L.Field a) => Matcher a m r -> Matcher L.Scalar m r
scalar = matchMaybe L.fromScalar

matchMaybe :: Monad m => (a -> Maybe b) -> Matcher b m r -> Matcher a m r
matchMaybe conv m = Matcher $ do
  s <- ask
  let mayS' = conv s
  case mayS' of
    Nothing -> mzero
    Just s' -> do
      mayR <- lift . lift $ runMatcher m s'
      maybe mzero return mayR

commodity :: Matcher Text m a -> Matcher L.Commodity m a
commodity = mapSubject (\(L.Commodity x) -> x)

serial :: Matcher L.Unsigned m r -> Matcher L.Serial m r
serial = mapSubject (\(L.Serial s) -> s)

forward :: Matcher L.Serial m r -> Matcher L.Forward m r
forward = mapSubject (\(L.Forward s) -> s)

reverse :: Matcher L.Serial m r -> Matcher L.Reverse m r
reverse = mapSubject (\(L.Reverse s) -> s)

recto :: Matcher L.Forward m r -> Matcher L.Serset m r
recto = mapSubject (\(L.Serset fwd _) -> fwd)

verso :: Matcher L.Reverse m r -> Matcher L.Serset m r
verso = mapSubject (\(L.Serset _ rev) -> rev)

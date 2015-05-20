{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Penny.Serial where

import Control.Lens
import Control.Applicative
import Penny.Natural
import qualified Data.Traversable as T
import Control.Monad.Trans.State
import Penny.Representation
import Data.Foldable (Foldable)
import Penny.Semantic

newtype Serial = Serial Unsigned
  deriving (Eq, Ord, Show, SemanticEq, SemanticOrd)

makeWrapped ''Serial

newtype Forward = Forward Serial
  deriving (Eq, Ord, Show)

makeWrapped ''Forward

newtype Backward = Backward Serial
  deriving (Eq, Ord, Show)

makeWrapped ''Backward

data Serset = Serset
  { _forward :: Forward
  , _backward :: Backward
  } deriving (Eq, Ord, Show)

makeLenses ''Serset

-- | Things that have a Serset.
data Sersetted a = Sersetted
  { _serset :: Serset
  , _sersetee :: a
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''Sersetted

assignSersetted :: T.Traversable t => t a -> t (Sersetted a)
assignSersetted t = flip evalState (toUnsigned Zero) $ do
  withFwd <- T.traverse (\a -> (,) <$> pure a <*> makeForward) t
  withBak <- T.traverse (\a -> (,) <$> pure a <*> makeBackward) withFwd
  let f ((b, fwd), bak) = Sersetted (Serset fwd bak) b
  return . fmap f $ withBak


makeForward :: State Unsigned Forward
makeForward = do
  this <- get
  modify next
  return $ Forward (Serial this)

makeBackward :: State Unsigned Backward
makeBackward = do
  old <- get
  let new = case prev old of
        Just x -> x
        Nothing -> error "makeBackward: error"
  put new
  return $ Backward (Serial new)


serialNumbers :: T.Traversable t => t a -> t (a, Serset)
serialNumbers t = flip evalState (toUnsigned Zero) $ do
  withFwd <- T.traverse (\a -> (,) <$> pure a <*> makeForward) t
  withBak <- T.traverse (\a -> (,) <$> pure a <*> makeBackward) withFwd
  let f ((b, fwd), bak) = (b, Serset fwd bak)
  return . fmap f $ withBak

serialNumbersNested
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 a)
  -> t1 (t2 (a, Serset))
serialNumbersNested t = flip evalState (toUnsigned Zero) $ do
  withFwd <-
    T.traverse (T.traverse (\a -> (,) <$> pure a <*> makeForward)) t
  withBak <-
    T.traverse (T.traverse (\a -> (,) <$> pure a <*> makeBackward)) withFwd
  let f ((b, fwd), bak) = (b, Serset fwd bak)
  return . fmap (fmap f) $ withBak


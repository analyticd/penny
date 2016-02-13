{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Penny.Serial where

import Control.Lens
import qualified Data.Traversable as T
import Control.Monad.Trans.State
import Data.Functor.Compose

import Penny.Digit
import Penny.Natural

data Serset = Serset
  { _forward :: Unsigned
  , _backward :: Unsigned
  } deriving (Eq, Ord, Show)

makeLenses ''Serset

data Serpack = Serpack
  { _file :: Serset
  , _global :: Serset
  } deriving (Eq, Ord, Show)

makeLenses ''Serpack

assignSersetted :: T.Traversable t => t a -> t (Serset, a)
assignSersetted t = flip evalState zero $ do
  withFwd <- T.traverse (\a -> (,) <$> pure a <*> makeForward) t
  withBak <- T.traverse (\a -> (,) <$> pure a <*> makeBackward) withFwd
  let f ((b, fwd), bak) = ((Serset fwd bak), b)
  return . fmap f $ withBak


makeForward :: State Unsigned Unsigned
makeForward = do
  this <- get
  modify next
  return this

makeBackward :: State Unsigned Unsigned
makeBackward = do
  old <- get
  let new = case prev old of
        Just x -> x
        Nothing -> error "makeBackward: error"
  put new
  return new


serialNumbers :: T.Traversable t => t a -> t (a, Serset)
serialNumbers t = flip evalState zero $ do
  withFwd <- traverse (\a -> (,) <$> pure a <*> makeForward) t
  withBak <- traverse (\a -> (,) <$> pure a <*> makeBackward) withFwd
  let f ((b, fwd), bak) = (b, Serset fwd bak)
  return . fmap f $ withBak

serialNumbersNested
  :: (T.Traversable t1, T.Traversable t2)
  => t1 (t2 a)
  -> t1 (t2 (a, Serset))
serialNumbersNested = getCompose . serialNumbers . Compose

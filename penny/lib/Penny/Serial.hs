{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Serial where

import Control.Lens
import qualified Data.Traversable as T
import Control.Monad.Trans.State
import Data.Functor.Compose
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

import Penny.NonNegative

data Serset = Serset
  { _forward :: NonNegative
  , _backward :: NonNegative
  } deriving (Eq, Ord, Show, Generic)

instance PrettyVal Serset

makeLenses ''Serset

data Sersetted a = Sersetted
  { _serset :: Serset
  , _sersetee :: a
  } deriving Generic

instance PrettyVal a => PrettyVal (Sersetted a)

makeLenses ''Sersetted

data Serpack = Serpack
  { _file :: Serset
  , _global :: Serset
  } deriving (Eq, Ord, Show, Generic)

instance PrettyVal Serpack

makeLenses ''Serpack

data Serpacked a = Serpacked
  { _serpack :: Serpack
  , _serpackee :: a
  } deriving Generic

instance PrettyVal a => PrettyVal (Serpacked a)

makeLenses ''Serpacked

assignSersetted :: T.Traversable t => t a -> t (Serset, a)
assignSersetted t = flip evalState zero $ do
  withFwd <- T.traverse (\a -> (,) <$> pure a <*> makeForward) t
  withBak <- T.traverse (\a -> (,) <$> pure a <*> makeBackward) withFwd
  let f ((b, fwd), bak) = ((Serset fwd bak), b)
  return . fmap f $ withBak


makeForward :: State NonNegative NonNegative
makeForward = do
  this <- get
  modify next
  return this

makeBackward :: State NonNegative NonNegative
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


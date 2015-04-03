{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Lincoln.Serial where

import Control.Applicative
import Penny.Lincoln.Natural
import qualified Data.Traversable as T
import Control.Monad.Trans.State
import Penny.Lincoln.Rep
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

newtype Serial = Serial Unsigned
  deriving (Eq, Ord, Show)

newtype Forward = Forward Serial
  deriving (Eq, Ord, Show)

newtype Reverse = Reverse Serial
  deriving (Eq, Ord, Show)

data Serset = Serset Forward Reverse
  deriving (Eq, Ord, Show)

-- | Things that have a Serset.
data Sersetted a = Sersetted Serset a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

--sersettedSerials :: T.Traversable t => t a -> t (Sersetted a)
--sersettedSerials = fmap (\(

makeForward :: State Unsigned Forward
makeForward = do
  this <- get
  modify next
  return $ Forward (Serial this)

makeReverse :: State Unsigned Reverse
makeReverse = do
  old <- get
  let new = case prev old of
        Nothing -> error "makeReverse: error"
        Just x -> x
  put new
  return $ Reverse (Serial new)


serialNumbers :: T.Traversable t => t a -> t (a, Serset)
serialNumbers t = flip evalState (toUnsigned Zero) $ do
  withFwd <- T.traverse (\a -> (,) <$> pure a <*> makeForward) t
  withBak <- T.traverse (\a -> (,) <$> pure a <*> makeReverse) withFwd
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
    T.traverse (T.traverse (\a -> (,) <$> pure a <*> makeReverse)) withFwd
  let f ((b, fwd), bak) = (b, Serset fwd bak)
  return . fmap (fmap f) $ withBak


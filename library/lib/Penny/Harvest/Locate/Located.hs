module Penny.Harvest.Locate.Located where

import qualified Penny.Core.Location as Location
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Control.Applicative

data T a = T
  { location :: Location.T
  , payload :: a
  } deriving (Eq, Ord, Show)

instance Functor T where
  fmap f (T l a) = T l (f a)

instance Foldable.Foldable T where
  foldr f z (T _ a) = f a z

instance Traversable.Traversable T where
  traverse g (T loc a) = (T loc) <$> g a

module Penny.Lincoln.Family.Siblings where

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as T
import Control.Applicative ((<*>), (<$>))

data Siblings a = Siblings { first :: a
                           , second :: a
                           , rest :: [a] }
                  deriving (Eq, Show)

instance Functor Siblings where
  fmap g (Siblings f s rs) = Siblings (g f) (g s) (map g rs)

instance Foldable.Foldable Siblings where
  foldr g b (Siblings f s rs) = g f (g s (foldr g b rs))

instance T.Traversable Siblings where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse g (Siblings f s rs) =
    Siblings
    <$> g f
    <*> g s
    <*> T.traverse g rs

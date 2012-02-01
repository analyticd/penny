module Penny.Lincoln.Family.Orphans where

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as T
import Control.Applicative ((<*>), (<$>))

data Orphans a = Orphans { first :: a
                           , second :: a
                           , rest :: [a] }
                  deriving Show

instance Functor Orphans where
  fmap g (Orphans f s rs) = Orphans (g f) (g s) (map g rs)

instance Foldable.Foldable Orphans where
  foldr g b (Orphans f s rs) = g f (g s (foldr g b rs))

instance T.Traversable Orphans where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse g (Orphans f s rs) =
    Orphans
    <$> g f
    <*> g s
    <*> T.traverse g rs

flatten :: Orphans a -> [a]
flatten (Orphans a1 a2 as) = a1:a2:as

others :: [a] -> [(a, [a])]
others = map yank . allIndexes

allIndexes :: [a] -> [(Int, [a])]
allIndexes as = zip [0..] (replicate (length as) as)

yank :: (Int, [a]) -> (a, [a])
yank (i, as) = let
  (ys, zs) = splitAt i as
  in (head zs, ys ++ (tail zs))

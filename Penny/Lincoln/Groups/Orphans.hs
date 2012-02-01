module Penny.Groups.AtLeast2 where

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as T
import Control.Applicative ((<*>), (<$>))

data AtLeast2 a = AtLeast2 { first :: a
                           , second :: a
                           , rest :: [a] }
                  deriving Show

instance Functor AtLeast2 where
  fmap g (AtLeast2 f s rs) = AtLeast2 (g f) (g s) (map g rs)

instance Foldable.Foldable AtLeast2 where
  foldr g b (AtLeast2 f s rs) = g f (g s (foldr g b rs))

instance T.Traversable AtLeast2 where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse g (AtLeast2 f s rs) =
    AtLeast2
    <$> g f
    <*> g s
    <*> T.traverse g rs

flatten :: AtLeast2 a -> [a]
flatten (AtLeast2 a1 a2 as) = a1:a2:as

others :: [a] -> [(a, [a])]
others = map yank . allIndexes

allIndexes :: [a] -> [(Int, [a])]
allIndexes as = zip [0..] (replicate (length as) as)

yank :: (Int, [a]) -> (a, [a])
yank (i, as) = let
  (ys, zs) = splitAt i as
  in (head zs, ys ++ (tail zs))

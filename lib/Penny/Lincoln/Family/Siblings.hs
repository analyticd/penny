module Penny.Lincoln.Family.Siblings where

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as T
import Control.Applicative ((<*>), (<$>))

data Siblings a = Siblings { first :: a
                         , second :: a
                         , rest :: [a] }
                  deriving Show

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

flatten :: Siblings a -> [a]
flatten (Siblings a1 a2 as) = a1:a2:as

others :: [a] -> [(a, [a])]
others = map yank . allIndexes

allIndexes :: [a] -> [(Int, [a])]
allIndexes as = zip [0..] (replicate (length as) as)

yank :: (Int, [a]) -> (a, [a])
yank (i, as) = let
  (ys, zs) = splitAt i as
  in (head zs, ys ++ (tail zs))

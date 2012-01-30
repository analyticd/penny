module Penny.Groups.AtLeast2 where

import qualified Penny.Groups.AtLeast1 as A1
import Penny.Groups.FamilyMember ( FamilyMember ( FamilyMember ) )
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

family :: AtLeast2 a -> [FamilyMember a]
family a = f : s : rs where
  f = FamilyMember (first a) (A1.AtLeast1 (second a) (rest a))
  s = FamilyMember (second a) (A1.AtLeast1 (first a) (rest a))
  rs = map toMember (others . rest $ a)
  toMember (m, as) =
    FamilyMember m (A1.AtLeast1 (first a) ((second a):as))

others :: [a] -> [(a, [a])]
others = map yank . allIndexes

allIndexes :: [a] -> [(Int, [a])]
allIndexes as = zip [0..] (replicate (length as) as)

yank :: (Int, [a]) -> (a, [a])
yank (i, as) = let
  (ys, zs) = splitAt i as
  in (head zs, ys ++ (tail zs))

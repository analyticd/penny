module Penny.NonEmpty where

import Penny.Natural
import Penny.Grammar (One(One))
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Sequence (Seq, (|>), viewl, ViewL(..), (<|))
import qualified Data.Sequence as S
import Data.Semigroup
import qualified Data.Map.Strict as M

data NonEmpty a = NonEmpty a (Seq a)
  deriving (Eq, Ord, Show)

seqFromNonEmpty :: NonEmpty a -> Seq a
seqFromNonEmpty (NonEmpty x xs) = x <| xs

nonEmptyLength :: NonEmpty a -> Positive
nonEmptyLength (NonEmpty _ ls) = F.foldl' go (toPositive One) ls
  where
    go old _ = next old

instance Semigroup (NonEmpty a) where
  (NonEmpty l ls) <> (NonEmpty r rs)
    = NonEmpty l ((ls |> r) <> rs)

prependSeq :: Seq a -> NonEmpty a -> NonEmpty a
prependSeq sq (NonEmpty a as) = case viewl sq of
  EmptyL -> NonEmpty a as
  x :< xs -> NonEmpty x (xs <> as)

instance Functor NonEmpty where
  fmap f (NonEmpty x xs) = NonEmpty (f x) (fmap f xs)

instance F.Foldable NonEmpty where
  foldr f z (NonEmpty x xs) = f x $ F.foldr f z xs

instance T.Traversable NonEmpty where
  traverse f (NonEmpty x xs)
    = NonEmpty <$> f x <*> T.traverse f xs

modeFromNonEmpty :: Ord a => NonEmpty a -> NonEmpty a
modeFromNonEmpty = finish . M.toList . F.foldl' fldr M.empty
  where
    fldr mp val = case M.lookup val mp of
      Nothing -> M.insert val 1 mp
      Just old -> M.insert val (succ old) mp
    finish pairs = case map fst . filter isMaxPair $ pairs of
      [] -> error "modeFromNonEmpty: error"
      x:xs -> NonEmpty x (S.fromList xs)
      where
        isMaxPair (_, v) = v == maxVal
          where
            maxVal = maximum . ((0 :: Integer) :) . map snd $ pairs

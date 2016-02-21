module Penny.NonEmpty where

import Control.Monad (ap)
import Penny.Natural
import Penny.Digit
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Sequence (Seq, (|>), viewl, ViewL(..), (<|))
import qualified Data.Sequence as S
import Data.Semigroup
import qualified Data.Map.Strict as M

data NonEmpty a = NonEmpty a (Seq a)
  deriving (Eq, Ord, Show)

instance Monad NonEmpty where
  return a = NonEmpty a S.empty
  (NonEmpty a as) >>= f = NonEmpty a'' as''
    where
      NonEmpty a'' front = f a
      back = fmap f as
      as'' = foldl add front back
        where
          add sq (NonEmpty b bs) = sq <> (b <| bs)

instance Applicative NonEmpty where
  pure = return
  (<*>) = ap

seqFromNonEmpty :: NonEmpty a -> Seq a
seqFromNonEmpty (NonEmpty x xs) = x <| xs

nonEmpty :: Seq a -> Maybe (NonEmpty a)
nonEmpty sq = case viewl sq of
  EmptyL -> Nothing
  x :< xs -> Just $ NonEmpty x xs

singleton :: a -> NonEmpty a
singleton a = NonEmpty a (S.empty)

nonEmptyLength :: NonEmpty a -> Positive
nonEmptyLength (NonEmpty _ ls) = F.foldl' go one ls
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

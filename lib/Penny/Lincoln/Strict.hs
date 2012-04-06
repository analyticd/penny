module Penny.Lincoln.Strict where

import qualified Data.Foldable as F
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Traversable as T
import Prelude hiding (zip, zipWith, unzip, length, replicate, splitAt,
                       fst, snd, concat, curry)
import qualified Prelude as P

data List a =
  !a :|: !(List a)
  | Empty
  deriving (Eq, Show, Ord)

null :: List a -> Bool
null Empty = True
null _ = False

concat :: List (List a) -> List a
concat = F.foldr f Empty where
  f ls soFar = ls `appendLists` soFar

splitAt :: Integral n => n -> List a -> Pair (List a) (List a)
splitAt n ls =
  if n <= fromIntegral (0 :: Int)
  then Empty :.: ls
  else case ls of
    Empty -> Empty :.: Empty
    (a :|: as) -> let
      r :.: rs = splitAt (pred n) as
      in (a :|: r) :.: rs

replicate :: Integral n => n -> a -> List a
replicate n ls =
  if n <= fromIntegral (0 :: Int)
  then Empty
  else ls :|: replicate (pred n) ls

appendLists :: List a -> List a -> List a
appendLists Empty l = l
appendLists (a :|: b) l = a :|: appendLists b l

data Pair a b =
  !a :.: !b
  deriving (Eq, Show, Ord)

toTuple :: Pair a b -> (a, b)
toTuple (a :.: b) = (a, b)

fst :: Pair a b -> a
fst (a :.: _) = a

snd :: Pair a b -> b
snd (_ :.: b) = b

curry :: ((Pair a b) -> c) -> a -> b -> c
curry f a b = f (a :.: b)

uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (a :.: b) = f a b

fromList :: [a] -> List a
fromList = foldr (:|:) Empty

lengthList :: Integral n => List a -> n
lengthList = F.foldl' f (fromIntegral (0 :: Int)) where
  f acc _ = succ acc

instance Functor List where
  fmap _ Empty = Empty
  fmap f (a :|: as) = f a :|: fmap f as

instance F.Foldable List where
  foldr _ acc Empty = acc
  foldr f acc (a :|: as) = f a (F.foldr f acc as)

instance T.Traversable List where
  traverse _ Empty = pure Empty
  traverse f (a :|: as) = (:|:) <$> f a <*> T.traverse f as

data NonEmpty a =
  !a :||: !(List a)
  deriving (Eq, Show, Ord)

lengthNonEmpty :: Integral n => NonEmpty a -> n
lengthNonEmpty (_ :||: as) =
  fromIntegral (1 :: Int) + lengthList as

unsafeFromList :: [a] -> NonEmpty a
unsafeFromList as = case as of
  (x:xs) -> x :||: fromList xs
  _ -> error "unsafeFromList: empty list"

instance Functor NonEmpty where
  fmap f (a :||: ls) = (f a :||: fmap f ls)

instance F.Foldable NonEmpty where
  foldr f acc (a :||: ls) = f a (F.foldr f acc ls)

instance T.Traversable NonEmpty where
  traverse f (a :||: ls) = (:||:) <$> f a <*> T.traverse f ls

infixr 5 :|:
infixr 5 :||:

data Might a =
  Nope
  | Here !a
  deriving (Eq, Show, Ord)

instance Functor Might where
  fmap _ Nope = Nope
  fmap f (Here a) = Here (f a)

toMaybe :: Might a -> Maybe a
toMaybe Nope = Nothing
toMaybe (Here a) = Just a

fromMaybe :: Maybe a -> Might a
fromMaybe Nothing = Nope
fromMaybe (Just a) = Here a

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith _ Empty _ = Empty
zipWith _ _ Empty = Empty
zipWith f (a :|: as) (b :|: bs) = f a b :|: zipWith f as bs

zip :: List a -> List b -> List (Pair a b)
zip = zipWith (:.:)

unzipWith :: (c -> (Pair a b)) -> List c -> (Pair (List a) (List b))
unzipWith _ Empty = (Empty :.: Empty)
unzipWith f (c :|: cs) = ((a :|: as) :.: (b :|: bs)) where
  (a :.: b) = f c
  (as :.: bs) = unzipWith f cs

unzip :: List (Pair a b) -> Pair (List a) (List b)
unzip = unzipWith id

concatNE :: List (NonEmpty a) -> List a
concatNE = F.foldr f Empty where
  f (a :||: as) soFar = (a :|: as) `appendLists` soFar

data Might a =
  Nope
  | Yep !a
  deriving (Eq, Show)

instance Functor Might where
  fmap _ Nope = Nope
  fmap f (Yep a) = Yep (f a)

{-# LANGUAGE DeriveGeneric #-}

-- | Anonymous sum types.

module Penny.Steel.Sums where

data S3 a b c
  = S3a a
  | S3b b
  | S3c c
  deriving (Eq, Ord, Show)

data S4 a b c d
  = S4a a
  | S4b b
  | S4c c
  | S4d d
  deriving (Eq, Ord, Show)

partitionS3 :: [S3 a b c] -> ([a], [b], [c])
partitionS3 = foldr f ([], [], [])
  where
    f i (as, bs, cs) = case i of
      S3a a -> (a:as, bs, cs)
      S3b b -> (as, b:bs, cs)
      S3c c -> (as, bs, c:cs)

partitionS4 :: [S4 a b c d] -> ([a], [b], [c], [d])
partitionS4 = foldr f ([], [], [], [])
  where
    f i (as, bs, cs, ds) = case i of
      S4a a -> (a:as, bs, cs, ds)
      S4b b -> (as, b:bs, cs, ds)
      S4c c -> (as, bs, c:cs, ds)
      S4d d -> (as, bs, cs, d:ds)

caseS3 :: (a -> d) -> (b -> d) -> (c -> d) -> S3 a b c -> d
caseS3 fa fb fc s3 = case s3 of
  S3a a -> fa a
  S3b b -> fb b
  S3c c -> fc c

caseS4 :: (a -> e) -> (b -> e) -> (c -> e) -> (d -> e) -> S4 a b c d -> e
caseS4 fa fb fc fd s4 = case s4 of
  S4a a -> fa a
  S4b b -> fb b
  S4c c -> fc c
  S4d d -> fd d

mapS3 :: (a -> a1) -> (b -> b1) -> (c -> c1) -> S3 a b c -> S3 a1 b1 c1
mapS3 fa fb fc = caseS3 (S3a . fa) (S3b . fb) (S3c . fc)

mapS4
  :: (a -> a1) -> (b -> b1) -> (c -> c1) -> (d -> d1)
  -> S4 a b c d
  -> S4 a1 b1 c1 d1
mapS4 a b c d = caseS4 (S4a . a) (S4b . b) (S4c . c) (S4d . d)

mapS3a
  :: Functor f
  => (a -> f a1) -> (b -> f b1) -> (c -> f c1) -> S3 a b c -> f (S3 a1 b1 c1)
mapS3a a b c = caseS3 (fmap S3a . a) (fmap S3b . b) (fmap S3c . c)

mapS4a
  :: Functor f
  => (a -> f a1) -> (b -> f b1) -> (c -> f c1) -> (d -> f d1)
  -> S4 a b c d -> f (S4 a1 b1 c1 d1)

mapS4a a b c d = caseS4 (fmap S4a . a) (fmap S4b . b) (fmap S4c . c)
                        (fmap S4d . d)


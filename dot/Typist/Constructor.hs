module Typist.Constructor where

data T a = T
  { name :: String
  , fields :: [a]
  } deriving (Eq, Ord, Show)

instance Functor T where
  fmap f (T n fs) = T n (fmap f fs)

empty :: String -> T a
empty s = T s []

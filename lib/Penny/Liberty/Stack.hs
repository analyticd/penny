module Penny.Liberty.Stack
       ( Stack, empty, push, View(Empty, Top),
         view) where

data Stack a = Stack [a]
               deriving (Eq, Read, Show, Ord)

instance Functor Stack where
  fmap f (Stack as) = Stack $ map f as

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push a (Stack as) = Stack (a:as)

data View t =
  Empty
  | Top (Stack t) t
  deriving Show

view :: Stack a -> View a
view (Stack as) = case as of
  [] -> Empty
  t:ts -> Top (Stack ts) t

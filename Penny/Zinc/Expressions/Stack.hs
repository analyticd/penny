module Penny.Zinc.Expressions.Stack
       ( Stack, empty, push, View(Empty, (:->)),
         view) where

data Stack a = Stack [a]
               deriving Show

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push a (Stack as) = Stack (a:as)

data View t =
  Empty
  | t :-> Stack t
  deriving Show

view :: Stack a -> View a
view (Stack as) = case as of
  [] -> Empty
  t:ts -> t :-> Stack ts

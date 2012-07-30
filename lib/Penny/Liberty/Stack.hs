-- | An efficient stack.
module Penny.Liberty.Stack
       ( Stack, empty, push, View(Empty, Top),
         view) where

data Stack a = Stack [a]
               deriving (Eq, Show)

instance Functor Stack where
  fmap f (Stack as) = Stack $ map f as

-- | An empty stack
empty :: Stack a
empty = Stack []

-- | Pushes an item onto the top of the stack.
push :: a -> Stack a -> Stack a
push a (Stack as) = Stack (a:as)

-- | Shows you the top of the stack.
data View t =
  Empty
  -- ^ The stack is empty

  | Top (Stack t) t
    -- ^ The stack has at least one item. Gives you the item and the
    -- rest of the stack with the item removed.
  deriving Show

view :: Stack a -> View a
view (Stack as) = case as of
  [] -> Empty
  t:ts -> Top (Stack ts) t

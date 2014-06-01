{-# LANGUAGE BangPatterns #-}
module Penny.Lincoln.Nats where

import Prelude hiding (length)
import Data.Semigroup

-- | Natural numbers, at least one.

data Positive = One | Succ !Positive
  deriving (Eq, Ord, Show)

addPositives :: Positive -> Positive -> Positive
addPositives x y = case x of
  One -> Succ y
  Succ x' -> addPositives x' (Succ y)

instance Semigroup Positive where
  (<>) = addPositives

positive :: Int -> Maybe Positive
positive i
  | i < 1 = Nothing
  | otherwise = Just $ go (i - 1) One
  where
    go !c n
      | c < 1 = n
      | otherwise = go (c - 1) (Succ n)

data NonNegative
  = Zero
  | Pos Positive
  deriving (Eq, Ord, Show)

length :: [a] -> NonNegative
length ls = case ls of
  [] -> Zero
  _:xs -> Pos $ go One xs
  where
    go acc xs = case xs of
      [] -> acc
      _:ys -> go (Succ acc) ys

instance Monoid NonNegative where
  mempty = Zero
  mappend x y = case (x, y) of
    (Zero, Zero) -> Zero
    (Pos x', Zero) -> Pos x'
    (Zero, Pos y') -> Pos y'
    (Pos x', Pos y') -> Pos $ x' <> y'

instance Semigroup NonNegative where
  (<>) = mappend

countPositive :: a -> (a -> a) -> Positive -> a
countPositive i f p = go i p
  where
    go !c pos = case pos of
      One -> c
      Succ pos' -> go (f c) pos'

countNonNegative :: a -> (a -> a) -> NonNegative -> a
countNonNegative i f p = case p of
  Zero -> i
  Pos pos -> countPositive (f i) f pos



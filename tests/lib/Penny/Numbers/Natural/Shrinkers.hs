module Penny.Numbers.Natural.Shrinkers where

import Penny.Numbers.Natural

pos :: Pos -> [Pos]
pos p = case p of
  One -> []
  Succ s -> [s]

nonNeg :: NonNeg -> [NonNeg]
nonNeg n = case n of
  Zero -> []
  NonZero p -> Zero : rest
    where
      rest = case p of
        One -> []
        Succ s -> [NonZero s]

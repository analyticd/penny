module Penny.Numbers.Natural.Coarbitrary where

import Penny.Numbers.Natural
import qualified Penny.Numbers.Natural as N
import Test.QuickCheck

pos :: Pos -> Gen b -> Gen b
pos p = case p of
  One -> variant (0 :: Int)
  Succ s -> variant (1 :: Int) . pos s

nonNeg :: NonNeg -> Gen b -> Gen b
nonNeg n = case n of
  Zero -> variant (0 :: Int)
  N.NonZero p -> variant (1 :: Int) . pos p

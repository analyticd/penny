module Penny.Numbers.Natural.Generators where

import Penny.Numbers.Natural
import qualified Penny.Numbers.Natural as N
import Test.QuickCheck

pos :: Gen Pos
pos = sized $ \s -> fmap f $ choose (0, s)
  where
    f n = foldr ($) One . replicate n $ Succ

nonNeg :: Gen NonNeg
nonNeg = oneof [ return Zero, fmap N.NonZero pos ]

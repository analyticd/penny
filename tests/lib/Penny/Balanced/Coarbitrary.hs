{-# LANGUAGE NoImplicitPrelude #-}
module Penny.Balanced.Coarbitrary where

import Penny.Balanced
import Penny.Ent.Coarbitrary
import Data.Sequence.Coarbitrary
import Test.QuickCheck

balanced
  :: (m -> Gen b -> Gen b)
  -> Balanced m
  -> Gen b
  -> Gen b
balanced b bl =
  seq (ent b) (balancedEnts bl)

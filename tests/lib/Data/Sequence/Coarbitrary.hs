module Data.Sequence.Coarbitrary where

import Data.Sequence
import Test.QuickCheck
import Prelude hiding (seq)

seq :: (a -> Gen b -> Gen b) -> Seq a -> Gen b -> Gen b
seq f s = case viewl s of
  EmptyL -> variant (0 :: Int)
  a :< rest -> variant (1 :: Int) . f a . seq f rest

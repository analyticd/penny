module Data.Sequence.Generators where

import Data.Sequence
import Test.QuickCheck
import Prelude hiding (seq)

seq :: Gen a -> Gen (Seq a)
seq = fmap fromList . listOf

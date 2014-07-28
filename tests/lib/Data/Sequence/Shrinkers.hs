module Data.Sequence.Shrinkers where

import Test.QuickCheck
import Data.Sequence
import Prelude hiding (seq)
import Data.Foldable

seq :: (a -> [a]) -> Seq a -> [Seq a]
seq f = map fromList . shrinkList f . toList

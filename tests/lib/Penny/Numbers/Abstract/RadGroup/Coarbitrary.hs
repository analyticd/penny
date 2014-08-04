module Penny.Numbers.Abstract.RadGroup.Coarbitrary where

import Penny.Numbers.Abstract.RadGroup hiding (grouper)
import Test.QuickCheck

radix :: Radix r -> Gen b -> Gen b
radix rdx = coarbitrary $ unRadix rdx

grouper :: Grouper a -> Gen b -> Gen b
grouper g = coarbitrary $ unGrouper g

group
  :: (b -> Gen r -> Gen r)
  -> Group a b
  -> Gen r
  -> Gen r
group f (Group g p) = grouper g . f p

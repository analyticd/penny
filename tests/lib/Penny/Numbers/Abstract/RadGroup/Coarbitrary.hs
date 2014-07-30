module Penny.Numbers.Abstract.RadGroup.Coarbitrary where

import Penny.Numbers.Abstract.RadGroup
import Test.QuickCheck

radix :: Radix r -> Gen b -> Gen b
radix rdx = coarbitrary $ unRadix rdx

group
  :: (b -> Gen r -> Gen r)
  -> Group a b
  -> Gen r
  -> Gen r
group f g = coarbitrary (grouper g) . f (groupPayload g)

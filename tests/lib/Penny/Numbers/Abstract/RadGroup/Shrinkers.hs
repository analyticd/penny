module Penny.Numbers.Abstract.RadGroup.Shrinkers where

import Penny.Numbers.Abstract.RadGroup

group
  :: (b -> [b])
  -- ^ Shrinks the payload
  -> Group a b
  -- ^ Group to shrink
  -> [Group a b]
group fs (Group g p) = map (Group g) . fs $ p

radix :: Radix r -> [Radix r]
radix _ = []

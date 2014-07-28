module Penny.Numbers.Abstract.RadGroup.Shrinkers where

import Penny.Numbers.Abstract.RadGroup

group
  :: (b -> [b])
  -- ^ Shrinks the payload
  -> (b -> Group a b)
  -- ^ Constructs members of the group
  -> Group a b
  -- ^ Group to shrink
  -> [Group a b]
group fs fk = map fk . fs . groupPayload

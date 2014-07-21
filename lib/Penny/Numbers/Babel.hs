-- | Conversion between Concrete to Abstract types.

module Penny.Numbers.Babel where

import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Concrete
import Deka.Dec (Sign)

fromConcrete
  :: (Sign -> p)
  -- ^ How to obtain the polarity.
  -> Radix r
  -> Concrete
  -> UngroupedPolar p r
fromConcrete = undefined

toConcrete
  :: (p -> Sign)
  -> UngroupedPolar p r
  -> Concrete
toConcrete = undefined

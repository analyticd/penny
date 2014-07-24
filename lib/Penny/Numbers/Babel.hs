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
  -> UngroupedPolar r p
fromConcrete = undefined

toConcrete
  :: (p -> Sign)
  -> UngroupedPolar r p
  -> Concrete
toConcrete = undefined

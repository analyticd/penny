module Penny.Numbers.Abstract.Gaggles where

import qualified Penny.Numbers.Abstract.Aggregates as A
import Penny.Numbers.Abstract.RadGroup
import Data.Sums
import Penny.Numbers.Qty

newtype UngroupedUnpolar = UngroupedUnpolar
  { unUngroupedUnpolar :: S2 (A.UngroupedUnpolar (Radix Period))
                             (A.UngroupedUnpolar (Radix Comma))
  } deriving (Eq, Ord, Show)

newtype UngroupedPolar = UngroupedPolar
  { unUngroupedPolar :: S2 (A.UngroupedPolar (Radix Period) Side)
                           (A.UngroupedPolar (Radix Comma) Side)
  } deriving (Eq, Ord, Show)

newtype GroupedNonZero = GroupedNonZero
  { unGroupedNonZero :: S4
    (A.GroupedNonZero Period Comma)
    (A.GroupedNonZero 
    

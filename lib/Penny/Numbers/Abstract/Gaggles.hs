module Penny.Numbers.Abstract.Gaggles where

import qualified Penny.Numbers.Abstract.Aggregates as A
import Penny.Numbers.Abstract.RadGroup
import Data.Sums
import Penny.Numbers.Qty

newtype UngroupedUnpolar = UngroupedUnpolar
  { unUngroupedUnpolar :: S2 (A.UngroupedUnpolar Period)
                             (A.UngroupedUnpolar Comma)
  } deriving (Eq, Ord, Show)

newtype UngroupedPolar p = UngroupedPolar
  { unUngroupedPolar :: S2 (A.UngroupedPolar Period p)
                           (A.UngroupedPolar Comma p)
  } deriving (Eq, Ord, Show)



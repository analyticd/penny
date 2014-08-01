module Penny.Serial.Coarbitrary where

import Penny.Serial
import Test.QuickCheck

serial :: Serial -> Gen b -> Gen b
serial s = variant (forward s) . variant (backward s)

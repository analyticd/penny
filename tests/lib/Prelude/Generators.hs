module Prelude.Generators where

import Prelude hiding (maybe)
import Test.QuickCheck

maybe :: Gen a -> Gen (Maybe a)
maybe g = frequency [(3, fmap Just g), (1, return Nothing)]

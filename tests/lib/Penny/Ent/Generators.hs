module Penny.Ent.Generators where

import Penny.Ent
import Test.QuickCheck
import Penny.Common.Generators
import Penny.Numbers.Abstract.Aggregates.Generators
import Control.Monad

-- | Generates 'Ent' using 'mkQCEnt'.
ent :: Gen m -> Gen (Ent m)
ent g = liftM4 mkQCEnt polarEitherRadix commodity arrangement g

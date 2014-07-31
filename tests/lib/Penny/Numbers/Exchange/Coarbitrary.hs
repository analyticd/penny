module Penny.Numbers.Exchange.Coarbitrary where

import qualified Penny.Numbers.Exchange as E
import Test.QuickCheck
import Penny.Numbers.Concrete.Coarbitrary
import Barecheck.Util

exch :: E.Exch -> Gen b -> Gen b
exch (E.Exch c) = concrete c

pluMin :: E.PluMin -> Gen b -> Gen b
pluMin s = case s of
  E.Plus -> varInt 0
  E.Minus -> varInt 1

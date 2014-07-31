module Penny.Numbers.Exchange.Generators where

import qualified Penny.Numbers.Exchange as E
import Test.QuickCheck
import Penny.Numbers.Concrete.Generators

exch :: Gen E.Exch
exch = fmap E.Exch concrete

pluMin :: Gen E.PluMin
pluMin = elements [ E.Plus, E.Minus ]

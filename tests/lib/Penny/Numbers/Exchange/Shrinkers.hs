module Penny.Numbers.Exchange.Shrinkers where

import qualified Penny.Numbers.Exchange as E
import Penny.Numbers.Concrete.Shrinkers

exch :: E.Exch -> [E.Exch]
exch = map E.Exch . concrete . E.unExch

pluMin :: E.PluMin -> [E.PluMin]
pluMin _ = []

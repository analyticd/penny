module Penny.Numbers.Qty.Shrinkers where

import qualified Penny.Numbers.Qty as Q
import Penny.Numbers.Concrete.Shrinkers

qty :: Q.Qty -> [Q.Qty]
qty = map Q.Qty . concrete . Q.unQty

side :: Q.Side -> [Q.Side]
side _ = []

module Penny.Balance.Coarbitrary where

import qualified Data.Map.Coarbitrary as M
import Test.QuickCheck
import Prelude hiding (map)
import Penny.Common.Coarbitrary
import Penny.Numbers.Qty.Coarbitrary
import Prelude.Coarbitrary
import Penny.Balance

map :: Balances -> Gen b -> Gen b
map (Balances m) = M.map (tuple2 commodity qty) m

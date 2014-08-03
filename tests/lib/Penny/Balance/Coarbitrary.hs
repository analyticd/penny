module Penny.Balance.Coarbitrary where

import qualified Data.Map.Coarbitrary as M
import Test.QuickCheck hiding (NonZero)
import Prelude hiding (map, exponent)
import Penny.Common.Coarbitrary
import Penny.Numbers.Qty.Coarbitrary
import Prelude.Coarbitrary
import Penny.Balance
import Penny.Numbers.Abstract.Unpolar.Coarbitrary

map :: Balances -> Gen b -> Gen b
map (Balances m) = M.map (tuple2 commodity qty) m

nonZero :: NonZero -> Gen b -> Gen b
nonZero (NonZero n e s) = novDecs n . exponent e . side s

imbalances :: Imbalances -> Gen b -> Gen b
imbalances (Imbalances m) = M.map (tuple2 commodity nonZero) m

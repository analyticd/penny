module Penny.Balance.Generators where

import Penny.Balance
import Test.QuickCheck hiding (NonZero)
import Data.Map.Generators
import Penny.Common.Generators
import Penny.Numbers.Qty.Generators
import Prelude hiding (map, exponent)
import Control.Monad
import Penny.Numbers.Abstract.Unpolar.Generators


balances :: Gen Balances
balances = fmap Balances $ map (listOf (liftM2 (,) commodity qty))

nonZero :: Gen NonZero
nonZero = liftM3 NonZero novDecs exponent side

imbalances :: Gen Imbalances
imbalances = fmap Imbalances $
  map (listOf (liftM2 (,) commodity nonZero))

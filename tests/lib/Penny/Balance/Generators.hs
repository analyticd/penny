module Penny.Balance.Generators where

import Penny.Balance
import Test.QuickCheck
import Data.Map.Generators
import Penny.Common.Generators
import Penny.Numbers.Qty.Generators
import Prelude hiding (map)
import Control.Monad

balances :: Gen Balances
balances = fmap Balances $ map (listOf (liftM2 (,) commodity qty))

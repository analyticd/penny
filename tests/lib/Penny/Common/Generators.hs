module Penny.Common.Generators where

import Penny.Common hiding (orient, spaceBetween)
import Data.Text.Generators
import Test.QuickCheck
import Control.Monad

commodity :: Gen Commodity
commodity = fmap Commodity $ text arbitrary

orient :: Gen Orient
orient = elements [ CommodityOnLeft, CommodityOnRight ]

spaceBetween :: Gen SpaceBetween
spaceBetween = elements [ SpaceBetween, NoSpaceBetween ]

arrangement :: Gen Arrangement
arrangement = liftM2 Arrangement orient spaceBetween

memo :: Gen Memo
memo = fmap Memo $ listOf (text arbitrary)

number :: Gen Number
number = fmap Number $ text arbitrary

payee :: Gen Payee
payee = fmap Payee $ text arbitrary

flag :: Gen Flag
flag = fmap Flag $ text arbitrary

line :: Gen Line
line = fmap Line arbitrary

filename :: Gen Filename
filename = fmap Filename $ text arbitrary

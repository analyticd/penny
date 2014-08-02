module Penny.TopLine.Generators where

import Penny.DateTime.Generators
import Penny.Common.Generators
import Penny.TopLine
import Test.QuickCheck
import Control.Monad

topLineData :: Gen TopLineData
topLineData = liftM5 TopLineData dateTime memo
  number flag payee



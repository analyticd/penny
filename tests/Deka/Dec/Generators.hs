module Deka.Dec.Generators where

import Test.QuickCheck
import Deka.Dec

posNeg :: Gen PosNeg
posNeg = elements [Pos, Neg]

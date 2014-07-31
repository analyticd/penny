module Penny.Common.Coarbitrary where

import Barecheck.Util
import Test.QuickCheck
import Penny.Common hiding (orient, spaceBetween)
import Data.Text.Coarbitrary
import Prelude.Coarbitrary

commodity :: Commodity -> Gen b -> Gen b
commodity (Commodity c) = text c

orient :: Orient -> Gen b -> Gen b
orient o = case o of
  CommodityOnLeft -> varInt 0
  CommodityOnRight -> varInt 1

spaceBetween :: SpaceBetween -> Gen b -> Gen b
spaceBetween s = case s of
  SpaceBetween -> varInt 0
  NoSpaceBetween -> varInt 1

arrangement :: Arrangement -> Gen b -> Gen b
arrangement (Arrangement o s)
  = orient o . spaceBetween s

memo :: Memo -> Gen b -> Gen b
memo (Memo ls) = list text ls

number :: Number -> Gen b -> Gen b
number (Number x) = text x

payee :: Payee -> Gen b -> Gen b
payee (Payee x) = text x

flag :: Flag -> Gen b -> Gen b
flag (Flag x) = text x

line :: Line -> Gen b -> Gen b
line (Line i) = varInt i

filename :: Filename -> Gen b -> Gen b
filename (Filename x) = text x

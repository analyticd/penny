module Penny.Common.Shrinkers where

import Penny.Common hiding (orient, spaceBetween)
import Data.Text.Shrinkers
import Test.QuickCheck
import Prelude.Shrinkers

commodity :: Commodity -> [Commodity]
commodity = fmap Commodity . text shrink . unCommodity

orient :: Orient -> [Orient]
orient _ = []

spaceBetween :: SpaceBetween -> [SpaceBetween]
spaceBetween _ = []

arrangement :: Arrangement -> [Arrangement]
arrangement (Arrangement o s) =
  [Arrangement o' s' | (o', s') <- tuple2 orient spaceBetween (o, s)]

memo :: Memo -> [Memo]
memo = fmap Memo . shrinkList (text shrink) . unMemo

number :: Number -> [Number]
number = fmap Number . text shrink . unNumber

payee :: Payee -> [Payee]
payee = fmap Payee . text shrink . unPayee

flag :: Flag -> [Flag]
flag = fmap Flag . text shrink . unFlag

line :: Line -> [Line]
line = fmap Line . shrinkIntegral . unLine

filename :: Filename -> [Filename]
filename = fmap Filename . text shrink . unFilename

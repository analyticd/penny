module Penny.Copper.ConvertAst where

import Penny.Lincoln
import Penny.Copper.Ast

decimalPlace :: (Digit a, Integral b) => Int -> a -> b
decimalPlace pl dig = digitToInt dig * 10 ^ pl

c'Int'DigitsFour :: Integral a => DigitsFour -> a
c'Int'DigitsFour (DigitsFour d3 d2 d1 d0)
  = decimalPlace 3 d3
  + decimalPlace 2 d2
  + decimalPlace 1 d1
  + decimalPlace 0 d0

c'Int'Digits1or2 :: Integral a => Digits1or2 -> a
c'Int'Digits1or2 (Digits1or2 l mayR) = case mayR of
  Nothing -> decimalPlace 0 l
  Just r -> decimalPlace 1 l + decimalPlace 0 r

c'Date'DateA :: DateA -> Maybe Date
c'Date'DateA (DateA y _ m _ d)
  = fromGregorian (c'Int'DigitsFour y)
                  (c'Int'Digits1or2 m)
                  (c'Int'Digits1or2 d)


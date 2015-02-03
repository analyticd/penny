module Penny.Copper.ConvertAst where

import Penny.Lincoln
import Penny.Copper.Ast
import Penny.Copper.Terminals
import Data.Text (Text)
import qualified Data.Text as X

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

c'Hours'HoursA :: HoursA -> Hours
c'Hours'HoursA x = case x of
  H0to19a m d -> H0to19 m d
  H20to23a _ d3 -> H20to23 d3

c'Time'TimeA :: TimeA -> Time
c'Time'TimeA (TimeA hA _ m mayS) = Time (c'Hours'HoursA hA) m s
  where
    s = case mayS of
      Nothing -> Seconds (ZeroTo59 Nothing D9z'0)
      Just (_, x) -> x

c'MaybeChar'EscSeq :: EscSeq -> Maybe Char
c'MaybeChar'EscSeq (EscSeq _ py) = case py of
  EscBackslash -> Just '\\'
  EscNewline -> Just '\n'
  EscQuote -> Just '"'
  EscGap _ _ -> Nothing

c'MaybeChar'QuotedChar :: QuotedChar -> Maybe Char
c'MaybeChar'QuotedChar (QuotedChar ei)
  = either (Just . termToChar) c'MaybeChar'EscSeq ei

c'Text'UnquotedString :: UnquotedString -> Text
c'Text'UnquotedString (UnquotedString digs nonDig ls) = undefined

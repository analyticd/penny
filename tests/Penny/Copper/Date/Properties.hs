{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Penny.Copper.Date.Properties where

import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Penny.Copper.Date.Instances
import Penny.Copper.Classes
import Penny.Copper.Parser
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Penny.Copper.Date
import Data.Maybe
import Penny.Lincoln

parseL :: ParserL a -> String -> Either String a
parseL p s
  | null ers = Right r
  | otherwise = Left (show ers)
  where
    (r, ersP, ersEnd) = parse ((,,) <$> p <*> pErrors <*> pEnd)
      (createStr (LineColPosA 0 0 0) s)
    ers = ersP ++ ersEnd

parseRenderParse
  :: (Show a, Renderable a)
  => ParserL a
  -> a
  -> Property
parseRenderParse prsr a =
  let ren1 = render a ""
      parse1 = parseL prsr ren1
  in case parse1 of
      Left e -> counterexample ("Parse 1 failed: Original: " ++ show a
        ++ " Rendered: " ++ show ren1
        ++ " errors: " ++ show e) (property False)
      Right g ->
        let ren2 = render g ""
        in counterexample ("Renders do not match. Original: " ++ show a
            ++ " Render1: " ++ show ren1
            ++ " Render2: " ++ show ren2) (ren1 == ren2)

prop_DateSep = parseRenderParse pDateSep
prop_One = parseRenderParse pOne
prop_Two = parseRenderParse pTwo
prop_Three = parseRenderParse pThree
prop_Four = parseRenderParse pFour
prop_Five = parseRenderParse pFive
prop_Six = parseRenderParse pSix
prop_Seven = parseRenderParse pSeven
prop_Eight = parseRenderParse pEight
prop_Nine = parseRenderParse pNine
prop_Days28 = parseRenderParse pDays28
prop_Days30 = parseRenderParse pDays30
prop_Days31 = parseRenderParse pDays31
prop_MonthDay = parseRenderParse pMonthDay
prop_Year = parseRenderParse pYear
prop_NonLeapDay = parseRenderParse pNonLeapDay
prop_Mod4 = parseRenderParse pMod4
prop_CenturyLeapYear = parseRenderParse pCenturyLeapYear
prop_NonCenturyLeapYear = parseRenderParse pNonCenturyLeapYear
prop_LeapDay = parseRenderParse pLeapDay
prop_DateA = parseRenderParse pDateA

prop_c'DateA'Day_succeeds (DayHP d) sep
  = isJust $ c'DateA'Day sep d

data DateAandDay = DateAandDay DayHP DateSep (Maybe DateA)
  deriving (Eq, Ord, Show)

instance Arbitrary DateAandDay where
  arbitrary = do
    DayHP dhp <- arbitrary
    sep <- arbitrary
    return $ DateAandDay (DayHP dhp) sep (c'DateA'Day sep dhp)

prop_DateA_and_Day_Isomorphic (DateAandDay (DayHP dhp) _ mDay)
  = case mDay of
      Nothing -> property False
      Just d'@(DateA ei) -> collect lbl $ c'Day'DateA d' == dhp
        where
          lbl = case ei of
            Left _ -> "regular day"
            Right _ -> "leap day"

newtype Int1to28 = Int1to28 Int
  deriving (Eq, Ord, Show)

instance Arbitrary Int1to28 where arbitrary = fmap Int1to28 (choose (1, 28))

prop_c'Days28'Int_succeeds (Int1to28 i) = isJust $ c'Days28'Int i

prop_dateAtoDayAndBack da sep = case c'DateA'Day sep (c'Day'DateA da) of
  Nothing -> property False
  Just da'@(DateA ei) -> collect lbl $ da' `semanticEq` da
    where
      lbl = case ei of
        Left _ -> "regular day"
        Right _ -> "leap day"

testGroup = $(testGroupGenerator)

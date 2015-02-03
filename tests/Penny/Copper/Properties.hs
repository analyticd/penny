{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Penny.Copper.Properties where

import Test.QuickCheck
import Penny.Copper.Classes
import Text.ParserCombinators.UU.Core hiding (parse)
import qualified Text.ParserCombinators.UU.Core
import Penny.Copper.Parser
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Penny.Copper.Ast
import Penny.Lincoln.Rep
import Penny.Lincoln.Instances ()
import Penny.Copper.Instances ()
import Penny.Copper.LincolnTypes
import Test.Tasty.QuickCheck
import Test.Tasty.TH

propertiesTestGroup = $(testGroupGenerator)

parse :: Parser a -> String -> (a, [Error LineColPosA])
parse prsr str = Text.ParserCombinators.UU.Core.parse
  ((,) <$> prsr <*> pEnd) (createStr (LineColPosA 0 0 0) str)

renParse
  :: (Eq a, Show a)
  => Parser a
  -> (a -> ShowS)
  -> a
  -> Property
renParse prsr rendr orig =
  let rendr1 = rendr orig ""
      (parsed, ers) = parse prsr rendr1
  in if not (null ers)
     then counterexample ("errors on first parse: " ++ show ers
            ++ " SOURCE STRING: " ++ rendr1) False
     else let rendr2 = rendr parsed ""
          in if rendr2 /= rendr1
             then counterexample
                  ("second render not equal. FIRST RENDER: " ++ show rendr1
                   ++ " SECOND RENDER: " ++ show rendr2) False
             else let rslt2 = parse prsr rendr2
                  in checkResult parsed rslt2

checkResult
  :: (Eq a, Show a) => a -> (a, [Error LineColPosA]) -> Property
checkResult orig (new, ls) = counterexample str $ null ls && orig == new
  where
    str = "ORIGINAL: " ++ show orig ++ " NEW: " ++ show new
      ++ " ERRORS: " ++ show ls

testR :: (Eq a, Show a, Parseable a, Renderable a) => a -> Property
testR = renParse parser render

testRRPer :: (Eq (a RadPer), Show (a RadPer), ParseableR a, RenderableR a)
  => a RadPer -> Property
testRRPer = renParse (parserR pRadixRadPer) (renderR rRadixRadPer)

testRRCom :: (Eq (a RadCom), Show (a RadCom), ParseableR a, RenderableR a)
  => a RadCom -> Property
testRRCom = renParse (parserR pRadixRadCom) (renderR rRadixRadCom)

testRGPer :: (Eq (a RadPer), Show (a RadPer), ParseableRG a, RenderableRG a)
  => a RadPer -> Property
testRGPer = renParse (parserRG pRadixRadPer pRadPer)
                     (renderRG rRadixRadPer rRadPer)

testRGCom :: (Eq (a RadCom), Show (a RadCom), ParseableRG a, RenderableRG a)
  => a RadCom -> Property
testRGCom = renParse (parserRG pRadixRadCom pRadCom)
                     (renderRG rRadixRadCom rRadCom)

testGCom :: (Eq (a RadCom), Show (a RadCom), ParseableG a, RenderableG a)
  => a RadCom -> Property
testGCom = renParse (parserG pRadCom) (renderG rRadCom)

testGPer :: (Eq (a RadPer), Show (a RadPer), ParseableG a, RenderableG a)
  => a RadPer -> Property
testGPer = renParse (parserG pRadPer) (renderG rRadPer)
--

prop_RadixRadCom :: Radix RadCom -> Property
prop_RadixRadCom = testR

prop_RadixRadPer :: Radix RadPer -> Property
prop_RadixRadPer = testR

prop_Grouper :: Grouper -> Property
prop_Grouper = testR

prop_RadCom :: RadCom -> Property
prop_RadCom = testR

prop_RadPer :: RadPer -> Property
prop_RadPer = testR

prop_Zero :: Zero -> Property
prop_Zero = testR

prop_nilUngroupedPer :: NilUngrouped RadPer -> Property
prop_nilUngroupedPer = testRRPer

prop_nilUngroupedCom :: NilUngrouped RadCom -> Property
prop_nilUngroupedCom = testRRCom

prop_nilGroupedCom :: NilGrouped RadCom -> Property
prop_nilGroupedCom = testRGCom

prop_nilGroupedPer :: NilGrouped RadPer -> Property
prop_nilGroupedPer = testRGPer

prop_nilPer :: Nil RadPer -> Property
prop_nilPer = testRGPer

prop_nilCom :: Nil RadCom -> Property
prop_nilCom = testRGCom

-- Brim

prop_BG8Per :: BG8 RadPer -> Property
prop_BG8Per = testGPer

prop_BG7Per :: BG7 RadPer -> Property
prop_BG7Per = testGPer

prop_BG6Per :: BG7 RadPer -> Property
prop_BG6Per = testGPer

prop_BG5Per :: BG7 RadPer -> Property
prop_BG5Per = testGPer

prop_BG1Per :: BG1 RadPer -> Property
prop_BG1Per = testRGPer

prop_BrimUngroupedPer :: BrimUngrouped RadPer -> Property
prop_BrimUngroupedPer = testRRPer

prop_BrimGroupedPer :: BrimGrouped RadPer -> Property
prop_BrimGroupedPer = testRGPer

prop_BrimPer :: Brim RadPer -> Property
prop_BrimPer = testRGPer

prop_NilOrBrimScalarPer :: Brim RadPer -> Property
prop_NilOrBrimScalarPer = testRGPer

prop_BG8Com :: BG8 RadCom -> Property
prop_BG8Com = testGCom

prop_BG7Com :: BG7 RadCom -> Property
prop_BG7Com = testGCom

prop_BG6Com :: BG7 RadCom -> Property
prop_BG6Com = testGCom

prop_BG5Com :: BG7 RadCom -> Property
prop_BG5Com = testGCom

prop_BG1Com :: BG1 RadCom -> Property
prop_BG1Com = testRGCom

prop_BrimUngroupedCom :: BrimUngrouped RadCom -> Property
prop_BrimUngroupedCom = testRRCom

prop_BrimGroupedCom :: BrimGrouped RadCom -> Property
prop_BrimGroupedCom = testRGCom

prop_BrimCom :: Brim RadCom -> Property
prop_BrimCom = testRGCom

prop_NilOrBrimScalarCom :: Brim RadCom -> Property
prop_NilOrBrimScalarCom = testRGCom

--

prop_Hash :: Hash -> Property
prop_Hash = testR

prop_Newline :: Newline -> Property
prop_Newline = testR

prop_Comment :: Comment -> Property
prop_Comment = testR

prop_DigitsFour :: DigitsFour -> Property
prop_DigitsFour = testR

prop_Digits1or2 :: Digits1or2 -> Property
prop_Digits1or2 = testR

prop_DateSep :: DateSep -> Property
prop_DateSep = testR

prop_DateA :: DateA -> Property
prop_DateA = testR

prop_Colon :: Colon -> Property
prop_Colon = testR

prop_TimeA :: TimeA -> Property
prop_TimeA = testR

prop_ZoneA :: ZoneA -> Property
prop_ZoneA = testR

prop_DoubleQuote :: DoubleQuote -> Property
prop_DoubleQuote = testR

prop_Backslash :: Backslash -> Property
prop_Backslash = testR

prop_White :: White -> Property
prop_White = testR

prop_Whites :: Whites -> Property
prop_Whites = testR

prop_EscPayload :: EscPayload -> Property
prop_EscPayload = testR

prop_EscSeq :: EscSeq -> Property
prop_EscSeq = testR

prop_QuotedChar :: QuotedChar -> Property
prop_QuotedChar = testR

prop_QuotedString :: QuotedString -> Property
prop_QuotedString = testR

prop_UnquotedString :: UnquotedString -> Property
prop_UnquotedString = testR

prop_UnquotedCommodityOnLeft :: UnquotedCommodityOnLeft -> Property
prop_UnquotedCommodityOnLeft = testR

prop_UnquotedCommodityOnRight :: UnquotedCommodityOnRight -> Property
prop_UnquotedCommodityOnRight = testR

prop_QuotedCommodity :: QuotedCommodity -> Property
prop_QuotedCommodity = testR

prop_CommodityA :: CommodityA -> Property
prop_CommodityA = testR

prop_CommodityOnLeft :: CommodityOnLeft -> Property
prop_CommodityOnLeft = testR

prop_CommodityOnRight :: CommodityOnRight -> Property
prop_CommodityOnRight = testR

prop_Backtick :: Backtick -> Property
prop_Backtick = testR

prop_NonNeutral :: NonNeutral -> Property
prop_NonNeutral = testR

prop_NeutralOrNon :: NeutralOrNon -> Property
prop_NeutralOrNon = testR

prop_TrioA :: TrioA -> Property
prop_TrioA = testR

prop_OpenSquare :: OpenSquare -> Property
prop_OpenSquare = testR

prop_CloseSquare :: CloseSquare -> Property
prop_CloseSquare = testR

prop_IntegerA :: IntegerA -> Property
prop_IntegerA = testR

prop_ScalarA :: ScalarA -> Property
prop_ScalarA = testR

prop_BracketedForest :: BracketedForest -> Property
prop_BracketedForest = testR

prop_ForestA :: ForestA -> Property
prop_ForestA = testR

prop_TreeA :: TreeA -> Property
prop_TreeA = testR

prop_TopLineA :: TopLineA -> Property
prop_TopLineA = testR

prop_PostingA :: PostingA -> Property
prop_PostingA = testR

prop_PostingsA :: PostingsA -> Property
prop_PostingsA = testR

prop_Semicolon :: Semicolon -> Property
prop_Semicolon = testR

prop_OpenCurly :: OpenCurly -> Property
prop_OpenCurly = testR

prop_CloseCurly :: CloseCurly -> Property
prop_CloseCurly = testR

prop_CommaA :: CommaA -> Property
prop_CommaA = testR

prop_PostingList :: PostingList -> Property
prop_PostingList = testR

prop_TransactionA :: TransactionA -> Property
prop_TransactionA = testR

prop_AtSign :: AtSign -> Property
prop_AtSign = testR

prop_PriceA :: PriceA -> Property
prop_PriceA = testR

prop_ExchA :: ExchA -> Property
prop_ExchA = testR

prop_FileItem :: FileItem -> Property
prop_FileItem = testR

prop_FileItems :: FileItems -> Property
prop_FileItems = testR

prop_File :: File -> Property
prop_File = testR

--
-- Other properties
--

--

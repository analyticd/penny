{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Penny.Copper.Properties where

--import Test.QuickCheck
import Penny.Copper.Classes
import Text.ParserCombinators.UU.Core
import Penny.Copper.Parser
import Text.ParserCombinators.UU.BasicInstances
import Penny.Copper.Ast
import Penny.Copper.Instances ()

testR :: (Eq a, Parseable a, Renderable a) => a -> Bool
testR a = null errors && r == a
  where
    (r, errors) = parse ((,) <$> parser <*> pEnd)
      (createStr (LineColPosA 0 0 0) (render a ""))

prop_Hash :: Hash -> Bool
prop_Hash = testR

prop_Newline :: Newline -> Bool
prop_Newline = testR

prop_Comment :: Comment -> Bool
prop_Comment = testR

prop_DigitsFour :: DigitsFour -> Bool
prop_DigitsFour = testR

prop_Digits1or2 :: Digits1or2 -> Bool
prop_Digits1or2 = testR

prop_DateSep :: DateSep -> Bool
prop_DateSep = testR

prop_DateA :: DateA -> Bool
prop_DateA = testR

prop_Colon :: Colon -> Bool
prop_Colon = testR

prop_TimeA :: TimeA -> Bool
prop_TimeA = testR

prop_ZoneA :: ZoneA -> Bool
prop_ZoneA = testR

prop_DoubleQuote :: DoubleQuote -> Bool
prop_DoubleQuote = testR

prop_Backslash :: Backslash -> Bool
prop_Backslash = testR

prop_White :: White -> Bool
prop_White = testR

prop_Whites :: Whites -> Bool
prop_Whites = testR

prop_EscPayload :: EscPayload -> Bool
prop_EscPayload = testR

prop_EscSeq :: EscSeq -> Bool
prop_EscSeq = testR

prop_QuotedChar :: QuotedChar -> Bool
prop_QuotedChar = testR

prop_QuotedString :: QuotedString -> Bool
prop_QuotedString = testR

prop_UnquotedString :: UnquotedString -> Bool
prop_UnquotedString = testR

prop_UnquotedCommodity :: UnquotedCommodity -> Bool
prop_UnquotedCommodity = testR

prop_QuotedCommodity :: QuotedCommodity -> Bool
prop_QuotedCommodity = testR

prop_CommodityA :: CommodityA -> Bool
prop_CommodityA = testR

prop_Backtick :: Backtick -> Bool
prop_Backtick = testR

prop_NonNeutral :: NonNeutral -> Bool
prop_NonNeutral = testR

prop_NeutralOrNon :: NeutralOrNon -> Bool
prop_NeutralOrNon = testR

prop_TrioA :: TrioA -> Bool
prop_TrioA = testR

prop_OpenSquare :: OpenSquare -> Bool
prop_OpenSquare = testR

prop_CloseSquare :: CloseSquare -> Bool
prop_CloseSquare = testR

prop_IntegerA :: IntegerA -> Bool
prop_IntegerA = testR

prop_ScalarA :: ScalarA -> Bool
prop_ScalarA = testR

prop_BracketedForest :: BracketedForest -> Bool
prop_BracketedForest = testR

prop_TreeA :: TreeA -> Bool
prop_TreeA = testR

prop_TopLineA :: TopLineA -> Bool
prop_TopLineA = testR

prop_PostingA :: PostingA -> Bool
prop_PostingA = testR

prop_PostingsA :: PostingsA -> Bool
prop_PostingsA = testR

prop_OpenCurly :: OpenCurly -> Bool
prop_OpenCurly = testR

prop_CloseCurly :: CloseCurly -> Bool
prop_CloseCurly = testR

prop_CommaA :: CommaA -> Bool
prop_CommaA = testR

prop_PostingList :: PostingList -> Bool
prop_PostingList = testR

prop_TransactionA :: TransactionA -> Bool
prop_TransactionA = testR

prop_AtSign :: AtSign -> Bool
prop_AtSign = testR

prop_PriceA :: PriceA -> Bool
prop_PriceA = testR

prop_ExchA :: ExchA -> Bool
prop_ExchA = testR

prop_FileItem :: FileItem -> Bool
prop_FileItem = testR

prop_FileItems :: FileItems -> Bool
prop_FileItems = testR

prop_File :: File -> Bool
prop_File = testR


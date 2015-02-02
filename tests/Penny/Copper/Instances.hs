{-# OPTIONS_GHC -fno-warn-orphans #-}

module Penny.Copper.Instances where

import Control.Applicative
import Test.QuickCheck
import Penny.Copper.Terminals
import Penny.Copper.Parser
import Penny.Copper.Intervals
import Data.DeriveTH
import Penny.Copper.Ast
import Penny.Lincoln.Instances ()

-- Terminals

pickFromMaybe :: Monad m => m (Maybe a) -> m a
pickFromMaybe g = do
  r <- g
  case r of
    Nothing -> pickFromMaybe g
    Just a -> return a

selectTerminal :: Arbitrary s => (s -> Maybe a) -> Gen a
selectTerminal f = pickFromMaybe (fmap f arbitrary)

instance Arbitrary CommentChar where
  arbitrary = selectTerminal commentChar

instance Arbitrary NonEscapedChar where
  arbitrary = selectTerminal nonEscapedChar

instance Arbitrary USCharNonDigit where
  arbitrary = selectTerminal usCharNonDigit

-- Parser

$(derive makeArbitrary ''LineColPosA)

-- AST

$(derive makeArbitrary ''Located)
$(derive makeArbitrary ''Fs)
$(derive makeArbitrary ''Bs)
$(derive makeArbitrary ''Hash)
$(derive makeArbitrary ''Newline)
$(derive makeArbitrary ''Comment)
$(derive makeArbitrary ''DigitsFour)
$(derive makeArbitrary ''Digits1or2)
$(derive makeArbitrary ''DateSep)
$(derive makeArbitrary ''DateA)
$(derive makeArbitrary ''Colon)
$(derive makeArbitrary ''TimeA)
$(derive makeArbitrary ''ZoneA)
$(derive makeArbitrary ''DoubleQuote)
$(derive makeArbitrary ''Backslash)
$(derive makeArbitrary ''White)
$(derive makeArbitrary ''Whites)
$(derive makeArbitrary ''EscPayload)
$(derive makeArbitrary ''EscSeq)
$(derive makeArbitrary ''QuotedChar)
$(derive makeArbitrary ''QuotedString)
$(derive makeArbitrary ''UnquotedString)
$(derive makeArbitrary ''UnquotedCommodityOnLeft)
$(derive makeArbitrary ''UnquotedCommodityOnRight)
$(derive makeArbitrary ''QuotedCommodity)
$(derive makeArbitrary ''CommodityA)
$(derive makeArbitrary ''CommodityOnLeft)
$(derive makeArbitrary ''CommodityOnRight)
$(derive makeArbitrary ''Backtick)
$(derive makeArbitrary ''NonNeutral)
$(derive makeArbitrary ''NeutralOrNon)
$(derive makeArbitrary ''TrioA)
$(derive makeArbitrary ''OpenSquare)
$(derive makeArbitrary ''CloseSquare)
$(derive makeArbitrary ''IntegerA)
$(derive makeArbitrary ''ScalarA)
$(derive makeArbitrary ''BracketedForest)
$(derive makeArbitrary ''ForestA)
$(derive makeArbitrary ''TreeA)
$(derive makeArbitrary ''TopLineA)
$(derive makeArbitrary ''PostingA)
$(derive makeArbitrary ''PostingsA)
$(derive makeArbitrary ''Semicolon)
$(derive makeArbitrary ''OpenCurly)
$(derive makeArbitrary ''CloseCurly)
$(derive makeArbitrary ''CommaA)
$(derive makeArbitrary ''PostingList)
$(derive makeArbitrary ''TransactionA)
$(derive makeArbitrary ''AtSign)
$(derive makeArbitrary ''PriceA)
$(derive makeArbitrary ''ExchA)
$(derive makeArbitrary ''FileItem)
$(derive makeArbitrary ''FileItems)
$(derive makeArbitrary ''File)

-- Intervals
instance (Arbitrary a, Ord a) => Arbitrary (Interval a) where
  arbitrary = oneof
    [ fmap singleton arbitrary
    , range <$> arbitrary <*> arbitrary
    ]

instance (Arbitrary a, Ord a) => Arbitrary (Intervals a) where
  arbitrary = Intervals <$> arbitrary <*> arbitrary

{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,
             MultiParamTypeClasses #-}
module Penny.Copper.Classes where

import Penny.Lincoln.Rep
import Penny.Lincoln.Side
import Penny.Lincoln.PluMin
import Penny.Copper.LincolnTypes
import Penny.Copper.Parser
import Penny.Copper.Ast
import Penny.Copper.Terminals

-- | Things that can be parsed.

class Parseable a where
  parser :: Parser a


-- | Things that can be parsed, but they must be passed a parser for
-- the radix point.
class ParseableR a where
  parserR :: Parser (Radix r) -> Parser (a r)

-- | Things that can be parsed, but they must be passed a parser for a
-- grouping character.

class ParseableG a where
  parserG :: Parser g -> Parser (a g)

-- | Things that can be parsed, but they must be passed a parser for a
-- grouping character and a parser for the radix point.

class ParseableRG a where
  parserRG :: Parser (Radix g) -> Parser g -> Parser (a g)

instance Parseable (Radix RadCom) where parser = pRadixRadCom
instance Renderable (Radix RadCom) where render = rRadixRadCom

instance Parseable (Radix RadPer) where parser = pRadixRadPer
instance Renderable (Radix RadPer) where render = rRadixRadPer

-- | Things that can be rendered.

class Renderable a where
  render :: a -> ShowS

-- | Things that can be rendered, but they must be passed a renderer
-- for the radix point.
class RenderableR a where
  renderR :: (Radix r -> ShowS) -> a r -> ShowS

-- | Things that can be rendered, but they must be passed a renderer
-- for a grouping character.

class RenderableG a where
  renderG :: (r -> ShowS) -> a r -> ShowS

-- | Things that can be rendered, but they must be passed renderers
-- for a grouping character and a radix point.
class RenderableRG a where
  renderRG :: (Radix r -> ShowS) -> (r -> ShowS) -> a r -> ShowS

instance Parseable Hash where parser = pHash
instance Renderable Hash where render = rHash

instance Parseable Newline where parser = pNewline
instance Renderable Newline where render = rNewline

instance Parseable Comment where parser = pComment
instance Renderable Comment where render = rComment

instance Parseable DigitsFour where parser = pDigitsFour
instance Renderable DigitsFour where render = rDigitsFour

instance Parseable Digits1or2 where parser = pDigits1or2
instance Renderable Digits1or2 where render = rDigits1or2

instance Parseable DateSep where parser = pDateSep
instance Renderable DateSep where render = rDateSep

instance Parseable DateA where parser = pDateA
instance Renderable DateA where render = rDateA

instance Parseable Colon where parser = pColon
instance Renderable Colon where render = rColon

instance Parseable TimeA where parser = pTimeA
instance Renderable TimeA where render = rTimeA

instance Parseable ZoneA where parser = pZoneA
instance Renderable ZoneA where render = rZoneA

instance Parseable DoubleQuote where parser = pDoubleQuote
instance Renderable DoubleQuote where render = rDoubleQuote

instance Parseable Backslash where parser = pBackslash
instance Renderable Backslash where render = rBackslash

instance Parseable White where parser = pWhite
instance Renderable White where render = rWhite

instance Parseable Whites where parser = pWhites
instance Renderable Whites where render = rWhites

instance Parseable EscPayload where parser = pEscPayload
instance Renderable EscPayload where render = rEscPayload

instance Parseable EscSeq where parser = pEscSeq
instance Renderable EscSeq where render = rEscSeq

instance Parseable QuotedChar where parser = pQuotedChar
instance Renderable QuotedChar where render = rQuotedChar

instance Parseable QuotedString where parser = pQuotedString
instance Renderable QuotedString where render = rQuotedString

instance Parseable UnquotedString where parser = pUnquotedString
instance Renderable UnquotedString where render = rUnquotedString

instance Parseable UnquotedCommodityOnLeft where
  parser = pUnquotedCommodityOnLeft
instance Renderable UnquotedCommodityOnLeft where
  render = rUnquotedCommodityOnLeft

instance Parseable UnquotedCommodityOnRight where
  parser = pUnquotedCommodityOnRight
instance Renderable UnquotedCommodityOnRight where
  render = rUnquotedCommodityOnRight

instance Parseable QuotedCommodity where parser = pQuotedCommodity
instance Renderable QuotedCommodity where render = rQuotedCommodity

instance Parseable CommodityA where parser = pCommodityA
instance Renderable CommodityA where render = rCommodityA

instance Parseable CommodityOnLeft where parser = pCommodityOnLeft
instance Renderable CommodityOnLeft where render = rCommodityOnLeft

instance Parseable CommodityOnRight where parser = pCommodityOnRight
instance Renderable CommodityOnRight where render = rCommodityOnRight

instance Parseable Backtick where parser = pBacktick
instance Renderable Backtick where render = rBacktick

instance Parseable NonNeutral where parser = pNonNeutral
instance Renderable NonNeutral where render = rNonNeutral

instance Parseable NeutralOrNon where parser = pNeutralOrNon
instance Renderable NeutralOrNon where render = rNeutralOrNon

instance Parseable TrioA where parser = pTrioA
instance Renderable TrioA where render = rTrioA

instance Parseable OpenSquare where parser = pOpenSquare
instance Renderable OpenSquare where render = rOpenSquare

instance Parseable CloseSquare where parser = pCloseSquare
instance Renderable CloseSquare where render = rCloseSquare

instance Parseable IntegerA where parser = pIntegerA
instance Renderable IntegerA where render = rIntegerA

instance Parseable ScalarA where parser = pScalarA
instance Renderable ScalarA where render = rScalarA

instance Parseable BracketedForest where parser = pBracketedForest
instance Renderable BracketedForest where render = rBracketedForest

instance Parseable ForestA where parser = pForestA
instance Renderable ForestA where render = rForestA

instance Parseable TreeA where parser = pTreeA
instance Renderable TreeA where render = rTreeA

instance Parseable TopLineA where parser = pTopLineA
instance Renderable TopLineA where render = rTopLineA

instance Parseable PostingA where parser = pPostingA
instance Renderable PostingA where render = rPostingA

instance Parseable PostingsA where parser = pPostingsA
instance Renderable PostingsA where render = rPostingsA

instance Parseable Semicolon where parser = pSemicolon
instance Renderable Semicolon where render = rSemicolon

instance Parseable OpenCurly where parser = pOpenCurly
instance Renderable OpenCurly where render = rOpenCurly

instance Parseable CloseCurly where parser = pCloseCurly
instance Renderable CloseCurly where render = rCloseCurly

instance Parseable CommaA where parser = pCommaA
instance Renderable CommaA where render = rCommaA

instance Parseable PostingList where parser = pPostingList
instance Renderable PostingList where render = rPostingList

instance Parseable TransactionA where parser = pTransactionA
instance Renderable TransactionA where render = rTransactionA

instance Parseable AtSign where parser = pAtSign
instance Renderable AtSign where render = rAtSign

instance Parseable PriceA where parser = pPriceA
instance Renderable PriceA where render = rPriceA

instance Parseable ExchA where parser = pExchA
instance Renderable ExchA where render = rExchA

instance Parseable FileItem where parser = pFileItem
instance Renderable FileItem where render = rFileItem

instance Parseable FileItems where parser = pFileItems
instance Renderable FileItems where render = rFileItems

instance Parseable File where parser = pFile
instance Renderable File where render = rFile

instance Parseable D9 where parser = pD9
instance Renderable D9 where render = rD9

instance Parseable D9z where parser = pD9z
instance Renderable D9z where render = rD9z

instance Parseable Grouper where parser = pGrouper
instance Renderable Grouper where render = rGrouper

instance Parseable RadCom where parser = pRadCom
instance Renderable RadCom where render = rRadCom

instance Parseable RadPer where parser = pRadPer
instance Renderable RadPer where render = rRadPer

instance Parseable Side where parser = pSide
instance Renderable Side where render = rSide

instance Parseable Zero where parser = pZero
instance Renderable Zero where render = rZero

instance Parseable PluMin where parser = pPluMin
instance Renderable PluMin where render = rPluMin

instance Parseable CommentChar where parser = pCommentChar
instance Renderable CommentChar where render = rCommentChar

instance Parseable NonEscapedChar where parser = pNonEscapedChar
instance Renderable NonEscapedChar where render = rNonEscapedChar

instance Parseable USCharNonDigit where parser = pUSCharNonDigit
instance Renderable USCharNonDigit where render = rUSCharNonDigit

instance ParseableG BG7 where parserG = pBG7
instance RenderableG BG7 where renderG = rBG7

instance ParseableG BG8 where parserG = pBG8
instance RenderableG BG8 where renderG = rBG8

instance ParseableG BG6 where parserG = pBG6
instance RenderableG BG6 where renderG = rBG6

instance ParseableG BG5 where parserG = pBG5
instance RenderableG BG5 where renderG = rBG5

instance ParseableRG BG1 where parserRG = pBG1
instance RenderableRG BG1 where renderRG = rBG1

instance ParseableRG BrimGrouped where parserRG = pBrimGrouped
instance RenderableRG BrimGrouped where renderRG = rBrimGrouped

instance ParseableRG Brim where parserRG = pBrim
instance RenderableRG Brim where renderRG = rBrim

instance ParseableRG Nil where parserRG = pNil
instance RenderableRG Nil where renderRG = rNil

instance ParseableRG NilOrBrimScalar where parserRG = pNilOrBrimScalar
instance RenderableRG NilOrBrimScalar where renderRG = rNilOrBrimScalar

instance ParseableRG NilGrouped where parserRG = pNilGrouped
instance RenderableRG NilGrouped where renderRG = rNilGrouped

instance ParseableR NilUngrouped where parserR = pNilUngrouped
instance RenderableR NilUngrouped where renderR = rNilUngrouped

instance ParseableR BrimUngrouped where parserR = pBrimUngrouped
instance RenderableR BrimUngrouped where renderR = rBrimUngrouped


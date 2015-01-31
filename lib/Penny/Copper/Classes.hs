{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes,
             MultiParamTypeClasses #-}
module Penny.Copper.Classes where

import Control.Applicative
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Core hiding (Zero)
import Penny.Lincoln.Rep
import Penny.Lincoln.Rep.Digits
import Penny.Lincoln.Side
import Penny.Lincoln.PluMin

-- | The 'LineColPos' that ships with uu-parsinglib is not an instance
-- of 'Eq'; having an 'Eq' instance can help enormously with testing
data LineColPosA = LineColPosA !Int !Int !Int
  deriving (Eq, Ord, Show)

instance IsLocationUpdatedBy LineColPosA Char where
  advance (LineColPosA lin ps ab) c = case c of
    '\n' -> LineColPosA (lin + 1) 0 (ab + 1)
    '\t' -> LineColPosA lin (ps + 8 - (ps - 1) `mod` 8) (ab + 1)
    _ -> LineColPosA lin (ps + 1) (ab + 1)

type Parser = P (Str Char String LineColPosA)

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


pNovem :: Parser Novem
pNovem =
  (D1 <$ pSym '1')
  <|> (D2 <$ pSym '2')
  <|> (D3 <$ pSym '3')
  <|> (D4 <$ pSym '4')
  <|> (D5 <$ pSym '5')
  <|> (D6 <$ pSym '6')
  <|> (D7 <$ pSym '7')
  <|> (D8 <$ pSym '8')
  <|> (D9 <$ pSym '9')

pDecem :: Parser Decem
pDecem = (D0 <$ pSym '0') <|> fmap Nonem pNovem

pGrouper :: Parser Grouper
pGrouper = ThinSpace <$ pSym '\x2009'
    <|> Underscore <$ pSym '_'

pRadCom :: Parser RadCom
pRadCom = Period <$ pSym '.'
    <|> RCGrouper <$> pGrouper

pRadixRadCom :: Parser (Radix RadCom)
pRadixRadCom = Radix <$ pSym ','

pRadPer :: Parser RadPer
pRadPer = Comma <$ pSym ','
    <|> RPGrouper <$> pGrouper

pRadixRadPer :: Parser (Radix RadPer)
pRadixRadPer = Radix <$ pSym '.'

pSeq :: Parser a -> Parser (Seq a)
pSeq p = fmap Seq.fromList $ many p

pSide :: Parser Side
pSide = Debit <$ pSym '<' <|> Credit <$ pSym '>'

pZero :: Parser Zero
pZero = Zero <$ pSym '0'

pSeqDecs :: Parser g -> Parser (Seq (g, Decem, Seq Decem))
pSeqDecs pg = pSeq ((,,) <$> pg <*> pDecem <*> pSeq pDecem)

pNilGrouped :: Parser (Radix r) -> Parser r -> Parser (NilGrouped r)
pNilGrouped pr pg =
  NilGrouped
  <$> optional pZero
  <*> pr
  <*> pZero
  <*> pSeq pZero
  <*> pg
  <*> pZero
  <*> pSeq pZero
  <*> pSeq ((,,) <$> pg <*> pZero <*> pSeq pZero)

pNilUngrouped :: Parser (Radix r) -> Parser (NilUngrouped r)
pNilUngrouped pr = pNUZero <|> pNURadix
  where
    pNUZero = NUZero
      <$> pZero
      <*> optional
            ((,) <$> pr
                 <*> optional ((,) <$> pZero <*> pSeq pZero))
    pNURadix = NURadix
      <$> pr
      <*> pZero
      <*> pSeq pZero

pBG7 :: Parser r -> Parser (BG7 r)
pBG7 pg = pz <|> pn
  where
    pz = BG7Zeroes
      <$> pZero
      <*> pSeq pZero
      <*> pBG8 pg
    pn = BG7Novem
      <$> pNovem
      <*> pSeq pDecem
      <*> pSeqDecs pg


pBG8 :: Parser r -> Parser (BG8 r)
pBG8 pg = pnv <|> pgrp
  where
    pnv = BG8Novem
      <$> pNovem
      <*> pSeq pDecem
      <*> pSeqDecs pg
    pgrp = BG8Group
      <$> pg
      <*> pBG7 pg

pBG6 :: Parser r -> Parser (BG6 r)
pBG6 pg = pnv <|> pgrp
  where
    pnv = BG6Novem
      <$> pNovem
      <*> pSeq pDecem
      <*> pg
      <*> pDecem
      <*> pSeq pDecem
      <*> pSeqDecs pg
    pgrp = BG6Group <$> pg <*> pBG7 pg

pBG5 :: Parser r -> Parser (BG5 r)
pBG5 pg = pnv <|> pz
  where
    pnv = BG5Novem
      <$> pNovem
      <*> pSeq pDecem
      <*> pg
      <*> pDecem
      <*> pSeq pDecem
      <*> pSeqDecs pg
    pz = BG5Zero
      <$> pZero
      <*> pSeq pZero
      <*> pBG6 pg

pBG1 :: Parser (Radix r) -> Parser r -> Parser (BG1 r)
pBG1 pr pg = onLeft <|> onRight
  where
    onLeft = BG1GroupOnLeft
      <$> pg
      <*> pDecem
      <*> pSeq pDecem
      <*> pSeqDecs pg
      <*> optional
          ( (,) <$> pr <*> optional
              (((,,) <$> pDecem <*> pSeq pDecem <*> pSeqDecs pg)))

    onRight = BG1GroupOnRight
      <$> pr
      <*> pDecem
      <*> pSeq pDecem
      <*> pSeqDecs pg

pBrimUngrouped :: Parser (Radix r) -> Parser (BrimUngrouped r)
pBrimUngrouped pr = gtOne <|> ltOne
  where
    gtOne = BUGreaterThanOne
      <$> pNovem
      <*> pSeq pDecem
      <*> optional ((,) <$> pr <*> pSeq pDecem)
    ltOne = BULessThanOne
      <$> optional pZero
      <*> pr
      <*> pSeq pZero
      <*> pNovem
      <*> pSeq pDecem

pBrimGrouped :: Parser (Radix r) -> Parser r -> Parser (BrimGrouped r)
pBrimGrouped pr pg = gtOne <|> ltOne
  where
    gtOne = BGGreaterThanOne
      <$> pNovem
      <*> pSeq pDecem
      <*> pBG1 pr pg -- BG1
    ltOne = BGLessThanOne
      <$> optional pZero
      <*> pr
      <*> pBG5 pg

pBrim :: Parser (Radix r) -> Parser r -> Parser (Brim r)
pBrim pr pg = BrimGrouped <$> pBrimGrouped pr pg
    <|> BrimUngrouped <$> pBrimUngrouped pr

pNil :: Parser (Radix r) -> Parser r -> Parser (Nil r)
pNil pr pg = NilU <$> pNilUngrouped pr
    <|> NilG <$> pNilGrouped pr pg

pNilOrBrimScalar
  :: Parser (Radix r)
  -> Parser r
  -> Parser (NilOrBrimScalar r)
pNilOrBrimScalar pr pg = fmap NilOrBrimScalar $ Left <$> pNil pr pg
    <|> Right <$> pBrim pr pg

pPluMin :: Parser PluMin
pPluMin = Plus <$ pSym '+' <|> Minus <$ pSym '-'

instance Parseable PluMin where
  parser = pPluMin

instance Parseable a => Parseable [a] where
  parser = many parser


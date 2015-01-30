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


instance Parseable Novem where
  parser =
    (D1 <$ pSym '1')
    <|> (D2 <$ pSym '2')
    <|> (D3 <$ pSym '3')
    <|> (D4 <$ pSym '4')
    <|> (D5 <$ pSym '5')
    <|> (D6 <$ pSym '6')
    <|> (D7 <$ pSym '7')
    <|> (D8 <$ pSym '8')
    <|> (D9 <$ pSym '9')

instance Parseable Decem where
  parser = (D0 <$ pSym '0') <|> fmap Nonem parser

instance Parseable Grouper where
  parser = ThinSpace <$ pSym '\x2009'
    <|> Underscore <$ pSym '_'

instance Parseable RadCom where
  parser = Period <$ pSym '.'
    <|> RCGrouper <$> parser

instance Parseable (Radix RadCom) where
  parser = fmap (const Radix) $ pSym ','

instance Parseable RadPer where
  parser = Comma <$ pSym ','
    <|> RPGrouper <$> parser

instance Parseable (Radix RadPer) where
  parser = fmap (const Radix) $ pSym '.'

instance Parseable a => Parseable (Seq a) where
  parser = fmap Seq.fromList $ many parser

instance Parseable Side where
  parser = Debit <$ pSym '<' <|> Credit <$ pSym '>'

instance Parseable Zero where
  parser = Zero <$ pSym '0'

instance ParseableRG NilGrouped where
  parserRG pr pg =
    NilGrouped
    <$> optional parser -- Zero
    <*> pr              -- radix
    <*> parser          -- Zero
    <*> parser          -- Seq Zero
    <*> pg              -- Grouper
    <*> parser          -- Zero
    <*> parser          -- Seq Zero
    <*> (Seq.fromList <$> many ((,,) <$> pg <*> parser <*> parser))

instance ParseableR NilUngrouped where
  parserR pr = pNUZero <|> pNURadix
    where
      pNUZero = NUZero
        <$> parser -- Zero
        <*> optional
              ((,) <$> pr
                   <*> optional ((,) <$> parser <*> parser))
      pNURadix = NURadix
        <$> pr
        <*> parser -- Zero
        <*> parser -- Seq Zero

instance ParseableG BG7 where
  parserG pg = pZero <|> pNovem
    where
      pZero = BG7Zeroes
        <$> parser     -- Zero
        <*> parser     -- Seq Zero
        <*> parserG pg -- BG8
      pNovem = BG7Novem
        <$> parser     -- Novem
        <*> parser     -- Seq Decem
        <*> fmap Seq.fromList
            (many ((,,) <$> pg <*> parser <*> parser))

instance ParseableG BG8 where
  parserG pg = pNovem <|> pGroup
    where
      pNovem = BG8Novem
        <$> parser -- Novem
        <*> parser -- Seq Decem
        <*> fmap Seq.fromList
            (many ((,,) <$> pg <*> parser <*> parser))
      pGroup = BG8Group
        <$> pg
        <*> parserG pg

instance ParseableG BG6 where
  parserG pg = pNovem <|> pGroup
    where
      pNovem = BG6Novem
        <$> parser -- Novem
        <*> parser -- Seq Decem
        <*> pg
        <*> parser -- Decem
        <*> parser -- Seq Decem
        <*> fmap Seq.fromList
            (many ((,,) <$> pg <*> parser <*> parser))
      pGroup = BG6Group <$> pg <*> parserG pg

instance ParseableG BG5 where
  parserG pg = pNovem <|> pZero
    where
      pNovem = BG5Novem
        <$> parser -- Novem
        <*> parser -- Seq Decem
        <*> pg
        <*> parser -- Decem
        <*> parser -- Seq Decem
        <*> fmap Seq.fromList
            (many ((,,) <$> pg <*> parser <*> parser))
      pZero = BG5Zero
        <$> parser -- Zero
        <*> parser -- Seq Zero
        <*> parserG pg -- BG6

instance ParseableRG BG1 where
  parserRG pr pg = onLeft <|> onRight
    where
      onLeft = BG1GroupOnLeft
        <$> pg
        <*> parser -- Decem
        <*> parser -- Seq Decem
        <*> (Seq.fromList <$> many ((,,) <$> pg <*> parser <*> parser))
        <*> optional
            ( (,) <$> pr <*> optional
                (((,,) <$> parser <*> parser <*>
                       ( Seq.fromList
                         <$> many ((,,) <$> pg <*> parser <*> parser)))))
      onRight = BG1GroupOnRight
        <$> pr
        <*> parser -- Decem
        <*> parser -- Seq Decem
        <*> fmap Seq.fromList
            (many ((,,) <$> pg <*> parser <*> parser))

instance ParseableR BrimUngrouped where
  parserR pr = gtOne <|> ltOne
    where
      gtOne = BUGreaterThanOne
        <$> parser -- Novem
        <*> parser -- Seq Decem
        <*> optional ((,) <$> pr <*> parser)
      ltOne = BULessThanOne
        <$> optional parser -- Zero
        <*> pr
        <*> parser -- Seq Zero
        <*> parser -- Novem
        <*> parser -- Seq Decem

instance ParseableRG BrimGrouped where
  parserRG pr pg = gtOne <|> ltOne
    where
      gtOne = BGGreaterThanOne
        <$> parser -- Novem
        <*> parser -- Seq Decem
        <*> parserRG pr pg -- BG1
      ltOne = BGLessThanOne
        <$> optional parser -- Zero
        <*> pr
        <*> parserG pg -- BG5

instance ParseableRG Brim where
  parserRG pr pg = BrimGrouped <$> parserRG pr pg
    <|> BrimUngrouped <$> parserR pr

instance ParseableRG Nil where
  parserRG pr pg = NilU <$> parserR pr
    <|> NilG <$> parserRG pr pg

instance ParseableRG NilOrBrimScalar where
  parserRG pr pg = fmap NilOrBrimScalar $ Left <$> parserRG pr pg
    <|> Right <$> parserRG pr pg

pPluMin :: Parser PluMin
pPluMin = Plus <$ pSym '+' <|> Minus <$ pSym '-'

instance Parseable PluMin where
  parser = pPluMin

instance Parseable a => Parseable [a] where
  parser = many parser


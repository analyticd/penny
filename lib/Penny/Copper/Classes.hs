{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes #-}
module Penny.Copper.Classes where

import Control.Applicative
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Core hiding (Zero)
import Penny.Lincoln.Rep
import Penny.Lincoln.Rep.Digits

type Parser = P (Str Char String LineColPos)

-- | Things that can be parsed.

class Parseable a where
  parser :: Parser a


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

instance Parseable Zero where
  parser = fmap (const Zero) $ pSym '0'

instance Parseable a => Parseable (Seq a) where
  parser = fmap Seq.fromList $ many parser


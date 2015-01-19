{-# LANGUAGE FlexibleInstances #-}
module Penny.Copper.Classes where

import Control.Applicative
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Parsec.Text
import Text.Parsec.Char
import Penny.Lincoln.Rep
import Penny.Lincoln.Rep.Digits

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
    (D1 <$ char '1')
    <|> (D2 <$ char '2')
    <|> (D3 <$ char '3')
    <|> (D4 <$ char '4')
    <|> (D5 <$ char '5')
    <|> (D6 <$ char '6')
    <|> (D7 <$ char '7')
    <|> (D8 <$ char '8')
    <|> (D9 <$ char '9')

instance Parseable Decem where
  parser = (D0 <$ char '0') <|> fmap Nonem parser

instance Parseable Grouper where
  parser = ThinSpace <$ char '\x2009'
    <|> Underscore <$ char '_'

instance Parseable RadCom where
  parser = Period <$ char '.'
    <|> RCGrouper <$> parser

instance Parseable (Radix RadCom) where
  parser = fmap (const Radix) $ char ','

instance Parseable RadPer where
  parser = Comma <$ char ','
    <|> RPGrouper <$> parser

instance Parseable (Radix RadPer) where
  parser = fmap (const Radix) $ char '.'

instance Parseable Zero where
  parser = fmap (const Zero) $ char '0'

instance Parseable a => Parseable (Seq a) where
  parser = fmap Seq.fromList $ many parser


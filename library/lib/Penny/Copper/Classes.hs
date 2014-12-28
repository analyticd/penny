{-# LANGUAGE FlexibleInstances #-}
module Penny.Copper.Classes where

import Control.Applicative
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Text.Parsec.Text
import Text.Parsec.Char
import Penny.Lincoln.Rep
import Penny.Lincoln.Rep.Digits
import qualified Penny.Lincoln.Natural as N
import Penny.Lincoln.Natural

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

instance Parseable RadCom where
  parser = fmap (const RadCom) $ char '.'

instance Parseable (Radix RadCom) where
  parser = fmap (const Radix) $ char ','

instance Parseable RadPer where
  parser = fmap (const RadPer) $ char ','

instance Parseable (Radix RadPer) where
  parser = fmap (const Radix) $ char '.'

instance Parseable Zero where
  parser = fmap (const Zero) $ char '0'

instance Parseable Zeroes where
  parser = f <$ char '0' <*> many (char '0')
    where
      f zs = Zeroes $ addUnsignedToPositive (fromNovem D1) (N.length zs)

instance Parseable a => Parseable (Seq a) where
  parser = fmap Seq.fromList $ many parser

instance Parseable Decems where
  parser = fmap Decems parser

instance Parseable DecDecs where
  parser = DecDecs <$> parser <*> parser

instance ParseableG DecDecsGrouped where
  parserG pg = DecDecsGrouped <$> pg <*> parser

instance ParseableG SeqDecDecsGrouped where
  parserG pg = fmap (SeqDecDecsGrouped . Seq.fromList)
    . many . parserG $ pg

instance ParseableG DecDecsMayGroups where
  parserG pg = DecDecsMayGroups <$> parser <*> parserG pg

instance ParseableRG BrimGrouped2 where
  parserRG pr pg = BrimGrouped2 <$> pr <*> optional (parserG pg)

instance ParseableRG BrimGrouped1 where
  parserRG pr pg =
    (BG1GroupOnLeft <$> pg <*> parserG pg <*> optional (parserRG pr pg))
    <|> (BG1GroupOnRight <$> pr <*> parser <*> parserG pg <*> parserG pg)

instance Parseable NovDecs where
  parser = NovDecs <$> parser <*> parser



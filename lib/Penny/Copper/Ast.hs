module Penny.Copper.Ast where

import Control.Applicative
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Core
import Penny.Copper.Classes
import Penny.Lincoln.Rep.Digits
import Penny.Copper.Intervals

data Located a = Located LineColPos a LineColPos
  deriving Show

instance Functor Located where
  fmap f (Located l1 a l2) = Located l1 (f a) l2

rangeToParser :: Intervals Char -> Parser Char
rangeToParser i = case intervalsToTuples i of
  [] -> error "rangeToParser: empty interval"
  xs -> foldl1 (<|>) . map pRange $ xs

commentChar :: Intervals Char
commentChar = included [range minBound maxBound]
  `remove` alone '\n'

newtype CommentChar = CommentChar Char
  deriving (Eq, Ord, Show)

data Comment = Comment [CommentChar]
  deriving (Eq, Ord, Show)

instance Parseable Comment where
  parser
    = Comment
    <$ pSym '#'
    <*> many (fmap CommentChar . rangeToParser $ commentChar)
    <* pSym '\n'

instance Parseable a => Parseable (Located a) where
  parser = Located <$> pPos <*> parser <*> pPos

data DigitsFour = DigitsFour Decem Decem Decem Decem
  deriving (Eq, Ord, Show)

instance Parseable DigitsFour where
  parser = DigitsFour <$> parser <*> parser <*> parser <*> parser

data Digits1or2 = Digits1or2 Decem (Maybe Decem)
  deriving (Eq, Ord, Show)

instance Parseable Digits1or2 where
  parser = Digits1or2 <$> parser <*> optional parser

data DateA = DateA DigitsFour Digits1or2 Digits1or2
  deriving (Eq, Ord, Show)

instance Parseable DateA where
  parser = DateA <$> parser <* dateSep
                 <*> parser <* dateSep <*> parser
    where
      dateSep = pSym '-' <|> pSym '/'

data TimeA = TimeA Digits1or2 Digits1or2 (Maybe Digits1or2)
  deriving (Eq, Ord, Show)

instance Parseable TimeA where
  parser = TimeA <$> parser <* colon <*> parser
    <*> optional (colon *> parser)
    where
      colon = pSym ':'

data ZonePlusMinus = ZonePlus | ZoneMinus
  deriving (Eq, Ord, Show)

instance Parseable ZonePlusMinus where
  parser = (ZonePlus <$ pSym '+') <|> (ZoneMinus <$ pSym '-')

data ZoneA = ZoneA ZonePlusMinus DigitsFour
  deriving (Eq, Ord, Show)

instance Parseable ZoneA where
  parser = ZoneA <$> parser <*> parser

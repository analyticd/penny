{-# LANGUAGE NoImplicitPrelude #-}
module Penny.Copper.Unpolar where

import Control.Applicative ((<$), (<$>), many, (<|>), (<*>))
import Deka.Native.Abstract (Novem(..), Decem(..))
import Prelude
  ( Eq
  , Ord
  , Show
  , undefined
  , ($)
  , fmap
  )
import Penny.Numbers.Concrete (NovDecs(..))
import Text.Parsec.Text (Parser)
--import Text.Parsec.Prim
import Text.Parsec.Char (char)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Penny.Numbers.Abstract.Unpolar (DecDecs(..))
import Penny.Numbers.Abstract.RadGroup
  ( Radix
  , Period(..)
  , Comma(..)
  , Grouper(..)
  , Group(Group)
  )
import qualified Penny.Numbers.Abstract.RadGroup as RG

data Zero = Zero
  deriving (Eq, Ord, Show)

-- # Sequence

sequence :: Parser a -> Parser (Seq a)
sequence p = fmap S.fromList $ many p

-- # Novem and Decem

novem :: Parser Novem
novem =
      D1 <$ char '1'
  <|> D2 <$ char '2'
  <|> D3 <$ char '3'
  <|> D4 <$ char '4'
  <|> D5 <$ char '5'
  <|> D6 <$ char '6'
  <|> D7 <$ char '7'
  <|> D8 <$ char '8'
  <|> D9 <$ char '9'

decem :: Parser Decem
decem =
      D0 <$ char '0'
  <|> Nonem <$> novem

novDecs :: Parser NovDecs
novDecs = NovDecs <$> novem <*> sequence decem

decDecs :: Parser DecDecs
decDecs = DecDecs <$> decem <*> sequence decem

-- # Radix and grouping

radPeriod :: Parser (Radix Period)
radPeriod = RG.radPeriod <$ char '.'

radComma :: Parser (Radix Comma)
radComma = RG.radComma <$ char ','

grouper :: Parser a -> Parser (Grouper a)
grouper p =
      Space  <$ char ' '
  <|> Thin   <$ char '\x2009'
  <|> Under  <$ char '_'
  <|> Unique <$> p

grouperComma :: Parser (Grouper Comma)
grouperComma = grouper (Period <$ char '.')

grouperPeriod :: Parser (Grouper Period)
grouperPeriod = grouper (Comma <$ char ',')

group :: Parser (Grouper a) -> Parser b -> Parser (Group a b)
group g p = Group <$> g <*> p


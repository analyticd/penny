module Penny.Copper.Number (isNumChar, number, render) where

import Control.Applicative ((<$>), (<*>))
import Data.Text ( pack, cons, snoc, Text )
import Text.Parsec ( char, satisfy, many, between, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (rangeLettersToSymbols)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )
import qualified Penny.Lincoln.TextNonEmpty as TNE

isNumChar :: Char -> Bool
isNumChar c = allowed && not banned where
  allowed = rangeLettersToSymbols c ||
            c == ' '
  banned = c == ')'

number :: Parser B.Number
number = between (char '(') (char ')') p <?> "number" where
  p = (\c cs -> B.Number (TextNonEmpty c (pack cs)))
      <$> satisfy isNumChar
      <*> many (satisfy isNumChar)

render :: B.Number -> Maybe Text
render (B.Number tne) =
  if TNE.all isNumChar tne
  then Just $ '(' `cons` TNE.toText tne `snoc` ')'
  else Nothing

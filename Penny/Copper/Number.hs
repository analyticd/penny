module Penny.Copper.Number where

import Control.Applicative ((<$>), (<*>))
import Data.Char ( isLetter, isNumber )
import qualified Data.Char as C
import Data.Text ( pack )
import Text.Parsec ( char, satisfy, many, between, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

numChar :: Char -> Bool
numChar c = allowed && not banned where
  allowed = inCat C.UppercaseLetter C.OtherSymbol c ||
            c == ' '
  banned = c == ')'

number :: Parser B.Number
number = between (char '(') (char ')') p <?> "number" where
  p = (\c cs -> B.Number (TextNonEmpty c (pack cs)))
      <$> satisfy numChar
      <*> many (satisfy numChar)


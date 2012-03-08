module Penny.Copper.Flag where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Char as C
import Data.Text ( pack )
import Text.Parsec ( char, satisfy, many, between, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

flagChar :: Char -> Bool
flagChar c = allowed && not banned where
  allowed = inCat C.UppercaseLetter C.OtherSymbol c ||
            c == ' '
  banned = c == ']'

flag :: Parser B.Flag
flag = between (char '[') (char ']') p <?> "flag" where
  p = (\c cs -> B.Flag (TextNonEmpty c (pack cs)))
       <$> satisfy flagChar
       <*> many (satisfy flagChar)

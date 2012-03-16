module Penny.Copper.Flag (flag, isFlagChar, render) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Char as C
import Data.Text ( pack, cons, snoc )
import qualified Data.Text as X
import Text.Parsec ( char, satisfy, many, between, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )
import qualified Penny.Lincoln.TextNonEmpty as TNE

isFlagChar :: Char -> Bool
isFlagChar c = allowed && not banned where
  allowed = inCat C.UppercaseLetter C.OtherSymbol c ||
            c == ' '
  banned = c == ']'

flag :: Parser B.Flag
flag = between (char '[') (char ']') p <?> "flag" where
  p = (\c cs -> B.Flag (TextNonEmpty c (pack cs)))
       <$> satisfy isFlagChar
       <*> many (satisfy isFlagChar)

render :: B.Flag -> Maybe X.Text
render (B.Flag fl) =
  if TNE.all isFlagChar fl
  then Just $ '[' `cons` (TNE.toText fl) `snoc` ']'
  else Nothing

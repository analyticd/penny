module Penny.Copper.Number where

import Control.Monad ( void )
import Data.Char ( isLetter, isNumber )
import Data.Text ( pack )
import Text.Parsec ( char, satisfy, manyTill )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

number :: Parser B.Number
number = do
  void $ char '('
  let p l =  isLetter l || isNumber l
  c <- satisfy p
  cs <- manyTill (satisfy p) (char ')')
  return . B.Number $ TextNonEmpty c (pack cs)


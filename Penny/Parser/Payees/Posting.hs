module Penny.Payees.Posting where

import Control.Monad ( void )
import Data.Char ( isLetter, isNumber, isPunctuation, isSymbol)
import Data.Text ( pack )
import Text.Parsec (
  char, satisfy, manyTill)
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits as B
import Penny.TextNonEmpty (TextNonEmpty ( TextNonEmpty ) )

postingPayee :: Parser B.Payee
postingPayee = do
  void $ char '<'
  let p c = notElem c "<>" && (isLetter c || isNumber c
            || isPunctuation c || isSymbol c || c == ' ')
  c <- satisfy p
  cs <- manyTill (satisfy p) (char '>')
  return . B.Payee $ TextNonEmpty c (pack cs)


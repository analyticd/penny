module Penny.Parser.Payees.Transaction where

import Control.Monad ( liftM )
import Data.Text ( pack )
import Text.Parsec ( manyTill, anyChar, noneOf, char )
import Text.Parsec.Text ( Parser )

import Penny.Bits (Payee (Payee))
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

payee :: Parser Payee
payee = do
  c <- anyChar
  rs <- liftM pack (manyTill (noneOf "\n") (char '\n'))
  return . Payee $ TextNonEmpty c rs

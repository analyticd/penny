module Penny.Parser.Flag where

import Control.Monad ( void )
import Data.Text ( pack )
import Text.Parsec ( char, satisfy, many )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits as B
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

flag :: Parser B.Flag
flag = do
  void $ char '['
  c <- satisfy (/= ']')
  cs <- many $ satisfy (/= ']')
  void $ char ']'
  return $ B.Flag (TextNonEmpty c (pack cs))


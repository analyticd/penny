module Penny.Parser.Flag where

import Control.Monad ( void )
import Text.Parsec ( char, anyChar )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits as B

flag :: Parser B.Flag
flag = do
  void $ char '['
  c <- anyChar
  void $ char ']'
  return $ B.Flag c


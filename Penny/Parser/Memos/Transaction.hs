module Penny.Parser.Memos.Transaction where

import Control.Monad ( void, liftM )
import Data.Text ( pack )
import Text.Parsec (
  char, many1, satisfy )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits as B
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

line :: Parser String
line = do
  void $ char ';'
  cs <- many1 (satisfy (/= '\n'))
  void $ char '\n'
  return (cs ++ "\n")

memo :: Parser B.Memo
memo = do
  (c:cs) <- liftM concat $ many1 line
  return . B.Memo $ TextNonEmpty c (pack cs)


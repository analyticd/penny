module Penny.Copper.Memos.Transaction where

import Control.Monad ( void, liftM )
import qualified Data.Char as C
import Data.Text ( pack )
import Text.Parsec (
  char, many1, satisfy, sourceLine, getPosition, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat)
import Penny.Lincoln.Meta (TopMemoLine(TopMemoLine), Line(Line))
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

isCommentChar :: Char -> Bool
isCommentChar c = inCat C.UppercaseLetter C.OtherSymbol c
                  || c == ' '

line :: Parser String
line = do
  void $ char ';'
  cs <- many1 (satisfy isCommentChar)
  void $ char '\n'
  return (cs ++ "\n")

memo :: Parser (B.Memo, TopMemoLine)
memo = p <?> "transaction memo" where
  p = do
    lin <- liftM (TopMemoLine . Line . sourceLine) getPosition
    (c:cs) <- liftM concat $ many1 line
    return ((B.Memo $ TextNonEmpty c (pack cs)), lin)


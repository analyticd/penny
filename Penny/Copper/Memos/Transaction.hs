module Penny.Copper.Memos.Transaction where

import Control.Applicative ((<*>), (<$>), (<*), (<$))
import qualified Data.Char as C
import Text.Parsec (
  char, many1, satisfy, sourceLine, getPosition, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, eol)
import Penny.Lincoln.Meta (TopMemoLine(TopMemoLine), Line(Line))
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty (unsafeTextNonEmpty)

isCommentChar :: Char -> Bool
isCommentChar c = inCat C.UppercaseLetter C.OtherSymbol c
                  || c == ' '

line :: Parser String
line = (++ "\n")
        <$ char ';'
        <*> many1 (satisfy isCommentChar)
        <* eol

-- | Parses a transaction memo and associated whitespace afterward.
memo :: Parser (B.Memo, TopMemoLine)
memo =
  (\lin ls -> (B.Memo (unsafeTextNonEmpty ls), lin))
  <$> ((TopMemoLine . Line . sourceLine) <$> getPosition)
  <*> (concat <$> many1 line)
  <?> "transaction memo"
  

module Penny.Copper.Memos.Transaction where

import Control.Applicative ((<*>), (<$>), (<*), (<$), optional)
import qualified Data.Char as C
import Data.Text (pack)
import Text.Parsec (
  char, satisfy, sourceLine, getPosition, (<?>),
  many)
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, eol)
import Penny.Lincoln.Meta (TopMemoLine(TopMemoLine), Line(Line))
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty (TextNonEmpty(TextNonEmpty))

isCommentChar :: Char -> Bool
isCommentChar c = inCat C.UppercaseLetter C.OtherSymbol c
                  || c == ' '

memoLine :: Parser B.MemoLine
memoLine = B.MemoLine <$> (
  TextNonEmpty
  <$ char ';'
  <* optional (char ' ')
  <*> satisfy isCommentChar
  <*> (pack <$> many (satisfy isCommentChar))
  <* eol )
  <?> "posting memo line"

-- | Parses a transaction memo and associated whitespace afterward.
memo :: Parser (B.Memo, TopMemoLine)
memo =
  flip (,)
  <$> ((TopMemoLine . Line . sourceLine) <$> getPosition)
  <*> (B.Memo <$> many memoLine)
  <?> "transaction memo"
  

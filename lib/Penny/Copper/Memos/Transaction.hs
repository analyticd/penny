module Penny.Copper.Memos.Transaction where

import Control.Applicative ((<*>), (<$>), (<*), (<$), optional)
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

memoLine :: Parser B.MemoLine
memoLine = B.MemoLine
           <$ char ';'
           <* optional (char ' ')
           <*> satisfy isCommentChar
           <*> many (satisfy isCommentChar)
           <* eol
           <?> "posting memo line"

memo :: Parser B.Memo
memo = B.Memo <$> many memoLine

-- | Parses a transaction memo and associated whitespace afterward.
memo :: Parser (B.Memo, TopMemoLine)
memo =
  flip (,)
  <$> ((TopMemoLine . Line . sourceLine) <$> getPosition)
  <*> memo
  <?> "transaction memo"
  

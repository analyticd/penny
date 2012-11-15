module Penny.Copper.Memos.Posting (
  memo, render, isMemoChar) where

import Control.Applicative ((<*), (*>))
import qualified Data.Text as X
import Text.Parsec (char, satisfy, many1, many, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (eol)
import qualified Penny.Copper.Util as U
import qualified Penny.Lincoln.Bits as B

isMemoChar :: Char -> Bool
isMemoChar c = U.unicodeAll c || U.asciiAll c

memo :: Parser B.Memo
memo = fmap (B.Memo . X.pack . concat) (many1 memoLine)

memoLine :: Parser String
memoLine = fmap (++ "\n") p
  where
    p = char '\''
        *> many (satisfy isMemoChar)
        <* eol
        <?> "posting memo"


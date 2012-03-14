module Penny.Copper.Memos.Posting where

import Control.Applicative ((<*>), (<*), (<$), optional, (<$>))
import qualified Data.Char as C
import Data.Text ( pack )
import Text.Parsec (char, satisfy, many, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, eol)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

isCommentChar :: Char -> Bool
isCommentChar c = inCat C.UppercaseLetter C.OtherSymbol c
                  || c == ' '

memo :: Parser B.Memo
memo = B.Memo <$> many memoLine

memoLine :: Parser B.MemoLine
memoLine = (\c cs -> B.MemoLine $ TextNonEmpty c (pack cs))
           <$ char '\''
           <* optional (char ' ')
           <*> satisfy isCommentChar
           <*> many (satisfy isCommentChar)
           <* eol
           <?> "posting memo"

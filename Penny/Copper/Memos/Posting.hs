module Penny.Copper.Memos.Posting where

import Control.Applicative ((<*>), (<*), (<$))
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
memo = (\c cs -> B.Memo $ TextNonEmpty c (pack cs))
       <$ char '\''
       <*> satisfy isCommentChar
       <*> many (satisfy isCommentChar)
       <* eol
       <?> "posting memo"

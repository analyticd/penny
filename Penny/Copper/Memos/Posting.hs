module Penny.Copper.Memos.Posting where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import qualified Data.Char as C
import Control.Monad ( liftM, void, when )
import Data.Text ( pack )
import Text.Parsec (
  try, many1, char, getParserState, satisfy,
  sourceColumn, statePos, noneOf, manyTill, many, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat)
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )


isCommentChar :: Char -> Bool
isCommentChar c = inCat C.UppercaseLetter C.OtherSymbol c
                  || c == ' '

memo :: Parser B.Memo
memo = let
  p1 = satisfy isCommentChar
  ps = many (satisfy isCommentChar)
  f c cs = B.Memo $ TextNonEmpty c (pack cs)
  in many (char ' ')
     *> char '\''
     *> (f <$> p1 <*> ps <?> "posting memo")
     <* char '\n'

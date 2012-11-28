module Penny.Copper.Memos.Posting (
  memo, render, isCommentChar) where

import Control.Applicative ((<*>), (<*), (<$), (<$>))
import qualified Data.Text as X
import Text.Parsec (char, satisfy, many, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (eol, rangeLettersToSymbols)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.TextNonEmpty as TNE

isCommentChar :: Char -> Bool
isCommentChar c = rangeLettersToSymbols c
                  || c == ' '

memo :: Parser B.Memo
memo = B.Memo <$> many memoLine

memoLine :: Parser B.MemoLine
memoLine = (\c cs -> B.MemoLine $ TNE.TextNonEmpty c (X.pack cs))
           <$ char '\''
           <*> satisfy isCommentChar
           <*> many (satisfy isCommentChar)
           <* eol
           <?> "posting memo"

render :: B.Memo -> Maybe X.Text
render (B.Memo ls) = X.concat <$> mapM renderLine ls 

renderLine :: B.MemoLine -> Maybe X.Text
renderLine (B.MemoLine l) =
  if TNE.all isCommentChar l
  then Just $
       X.pack (replicate 8 ' ')
       `X.snoc` '\''
       `X.append` TNE.toText l
       `X.snoc` '\n'
  else Nothing

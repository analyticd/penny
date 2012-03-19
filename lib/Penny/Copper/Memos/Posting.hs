module Penny.Copper.Memos.Posting (
  memo, render, isCommentChar) where

import Control.Applicative ((<*>), (<*), (<$), (<$>))
import qualified Data.Char as C
import qualified Data.Text as X
import Text.Parsec (char, satisfy, many, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, eol)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.TextNonEmpty as TNE

isCommentChar :: Char -> Bool
isCommentChar c = inCat C.UppercaseLetter C.OtherSymbol c
                  || c == ' '

memo :: Parser B.Memo
memo = B.Memo <$> many memoLine

memoLine :: Parser B.MemoLine
memoLine = (\c cs -> B.MemoLine $ TNE.TextNonEmpty c (X.pack cs))
           <$ many (char ' ')
           <* char '\''
           <*> satisfy isCommentChar
           <*> many (satisfy isCommentChar)
           <* eol
           <?> "posting memo"

render :: B.Memo -> Maybe X.Text
render (B.Memo ls) = X.concat `fmap` mapM renderLine ls 

renderLine :: B.MemoLine -> Maybe X.Text
renderLine (B.MemoLine l) =
  if TNE.all isCommentChar l
  then Just $ X.pack "    '" `X.append` TNE.toText l `X.snoc` '\n'
  else Nothing

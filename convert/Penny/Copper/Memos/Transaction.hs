module Penny.Copper.Memos.Transaction (
  memo, render, isCommentChar
  ) where

import Control.Applicative ((<*>), (<$>), (<*), (<$))
import qualified Data.Text as X
import Text.Parsec (
  char, satisfy, sourceLine, getPosition, (<?>),
  many)
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (rangeLettersToSymbols, eol)
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.TextNonEmpty as TNE

isCommentChar :: Char -> Bool
isCommentChar c = rangeLettersToSymbols c
                  || c == ' '

memoLine :: Parser L.MemoLine
memoLine = L.MemoLine <$> (
  TNE.TextNonEmpty
  <$ char ';'
  <*> satisfy isCommentChar
  <*> (X.pack <$> many (satisfy isCommentChar))
  <* eol )
  <?> "posting memo line"

-- | Parses a transaction memo and associated whitespace afterward.
memo :: Parser (L.Memo, L.TopMemoLine)
memo =
  flip (,)
  <$> ((L.TopMemoLine . sourceLine) <$> getPosition)
  <*> (L.Memo <$> many memoLine)
  <?> "transaction memo"
  
-- | Renders a transaction memo. Fails if the memo is not renderable.
render :: L.Memo -> Maybe X.Text
render (L.Memo m) = X.concat <$> mapM renderLine m

renderLine :: L.MemoLine -> Maybe X.Text
renderLine (L.MemoLine l) =
  if TNE.all isCommentChar l
  then Just $ X.singleton ';' `X.append` TNE.toText l `X.snoc` '\n'
  else Nothing

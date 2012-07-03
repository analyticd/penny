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
import qualified Penny.Copper.Meta as M
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.TextNonEmpty as TNE

isCommentChar :: Char -> Bool
isCommentChar c = rangeLettersToSymbols c
                  || c == ' '

memoLine :: Parser B.MemoLine
memoLine = B.MemoLine <$> (
  TNE.TextNonEmpty
  <$ char ';'
  <*> satisfy isCommentChar
  <*> (X.pack <$> many (satisfy isCommentChar))
  <* eol )
  <?> "posting memo line"

-- | Parses a transaction memo and associated whitespace afterward.
memo :: Parser (B.Memo, M.TopMemoLine)
memo =
  flip (,)
  <$> ((M.TopMemoLine . sourceLine) <$> getPosition)
  <*> (B.Memo <$> many memoLine)
  <?> "transaction memo"
  
-- | Renders a transaction memo. Fails if the memo is not renderable.
render :: B.Memo -> Maybe X.Text
render (B.Memo m) = X.concat <$> mapM renderLine m

renderLine :: B.MemoLine -> Maybe X.Text
renderLine (B.MemoLine l) =
  if TNE.all isCommentChar l
  then Just $ X.singleton ';' `X.append` TNE.toText l `X.snoc` '\n'
  else Nothing

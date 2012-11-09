module Penny.Copper.Memos.Transaction (
  memo, render, isMemoChar
  ) where

import Control.Applicative ((<*>), (<*), (*>), (<$>))
import qualified Data.Text as X
import Text.Parsec (
  char, satisfy, sourceLine, getPosition, (<?>),
  many, many1)
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (eol)
import qualified Penny.Copper.Util as U
import qualified Penny.Lincoln as L

isMemoChar :: Char -> Bool
isMemoChar c = U.unicodeAll c || U.asciiAll c


memoLine :: Parser String
memoLine = fmap (++ "\n") p
  where
    p = char ';'
        *> many (satisfy isMemoChar)
        <* eol
        <?> "posting memo line"

memoText :: Parser L.Memo
memoText = fmap (L.Memo . X.pack . concat) (many1 memoLine)

-- | Parses a transaction memo and associated whitespace afterward.
memo :: Parser (L.Memo, L.TopMemoLine)
memo =
  flip (,)
  <$> ((L.TopMemoLine . sourceLine) <$> getPosition)
  <*> memoText
  <?> "transaction memo"


-- | Renders a transaction memo. Fails if the memo is not renderable.
render :: L.Memo -> Maybe X.Text
render (L.Memo x) =
  let ls = X.split (== '\n') x
  in if null ls || (not (all (X.all isMemoChar) ls))
     then Nothing
     else
      let mkLn l = X.singleton ';' `X.append` l `X.snoc` '\n'
      in Just . X.concat . map mkLn $ ls

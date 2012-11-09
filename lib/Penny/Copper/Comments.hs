module Penny.Copper.Comments (
  Comment(Comment)
  , comment
  , isCommentChar
  , render
  ) where

import Control.Applicative ((<*>), (<*), (<$))
import Data.Text ( Text, pack, cons, snoc )
import qualified Data.Text as X
import Text.Parsec (many, char, satisfy, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (eol)
import qualified Penny.Copper.Util as U

data Comment = Comment Text
             deriving (Eq, Show)

isCommentChar :: Char -> Bool
isCommentChar c = U.asciiAll c || U.unicodeAll c


comment :: Parser Comment
comment = Comment . pack
          <$ char '#'
          <*> many (satisfy isCommentChar)
          <* eol
          <?> "single line comment"

render :: Comment -> Maybe Text
render (Comment t) = case X.find (not . isCommentChar) t of
  Nothing -> return $ '#' `cons` t `snoc` '\n'
  Just _ -> Nothing

module Penny.Copper.Comments (
  Comment(Comment)
  , comment
  , isCommentChar
  , render
  ) where

import Control.Applicative ((<*>), (<*), (<$))
import qualified Data.Char as C
import Data.Text ( Text, pack, cons, snoc )
import qualified Data.Text as X
import Text.Parsec (many, char, satisfy, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, eol)

data Comment = Comment Text
             deriving (Eq, Show)

isCommentChar :: Char -> Bool
isCommentChar c = inCategory || allowed where
  allowed = c `elem` " "
  inCategory = inCat C.UppercaseLetter C.OtherSymbol c

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

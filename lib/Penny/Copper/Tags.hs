module Penny.Copper.Tags (
  isTagChar, tags, render
  )where

import Control.Applicative ((<$>), (*>))
import qualified Data.Text as X
import Text.Parsec (char, satisfy, many1, many, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (lexeme)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Util as U

isTagChar :: Char -> Bool
isTagChar c = U.unicodeAll c || U.asciiAll c

tagChar :: Parser Char
tagChar = satisfy isTagChar

tag :: Parser B.Tag
tag = (char '*' *> ((B.Tag . X.pack) <$> many1 tagChar)) <?> e where
  e = "tag"

tags :: Parser B.Tags
tags = B.Tags <$> many (lexeme tag)

renderTag :: B.Tag -> Maybe X.Text
renderTag (B.Tag t) =
  if X.all isTagChar t
  then Just $ X.cons '*' t
  else Nothing

render :: B.Tags -> Maybe X.Text
render (B.Tags ts) =
  X.intercalate (X.singleton ' ')
  <$> mapM renderTag ts

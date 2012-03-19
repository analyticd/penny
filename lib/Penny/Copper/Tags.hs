module Penny.Copper.Tags (isTagChar, tags) where

import Control.Applicative ((<$>), (*>), (<*>))
import qualified Data.Char as C
import qualified Data.Text as X
import Text.Parsec (char, satisfy, many, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, lexeme)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.TextNonEmpty as TNE

isTagChar :: Char -> Bool
isTagChar = inCat C.UppercaseLetter C.OtherNumber

tagChar :: Parser Char
tagChar = satisfy isTagChar

tag :: Parser B.Tag
tag = (char '#' *> (f <$> tagChar <*> many tagChar)) <?> e where
  f t ts = B.Tag $ TNE.TextNonEmpty t (X.pack ts)
  e = "tag"

tags :: Parser B.Tags
tags = B.Tags <$> many (lexeme tag)

renderTag :: B.Tag -> Maybe X.Text
renderTag (B.Tag t) =
  if TNE.all isTagChar t
  then Just $ X.cons '#' (TNE.toText t)
  else Nothing

render :: B.Tags -> Maybe X.Text
render (B.Tags ts) = do
  

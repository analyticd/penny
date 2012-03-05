module Penny.Copper.Tags where

import Control.Applicative ((<$>), (*>), (<*>))
import Control.Monad ( liftM )
import Data.Char ( isLetter, isNumber )
import qualified Data.Char as C
import Data.Text ( pack )
import Text.Parsec (
  char, satisfy, many, skipMany, try, (<?>))
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, lexeme)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

isTagChar :: Char -> Bool
isTagChar = inCat C.UppercaseLetter C.OtherNumber

tagChar :: Parser Char
tagChar = satisfy isTagChar

tag :: Parser B.Tag
tag = (char '#' *> (f <$> tagChar <*> many tagChar)) <?> e where
  f t ts = B.Tag $ TextNonEmpty t (pack ts)
  e = "tag"

tags :: Parser B.Tags
tags = B.Tags <$> many (lexeme tag)



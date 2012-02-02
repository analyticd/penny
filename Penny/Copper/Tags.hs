module Penny.Copper.Tags where

import Control.Monad ( liftM )
import Data.Char ( isLetter, isNumber )
import Data.Text ( pack )
import Text.Parsec (
  char, satisfy, many, skipMany, try )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

tagChar :: Parser Char
tagChar = satisfy (\l -> isLetter l || isNumber l)

tag :: Parser B.Tag
tag = do
  _ <- char '#'
  f <- tagChar
  r <- liftM pack (many tagChar)
  return . B.Tag $ TextNonEmpty f r

firstTag :: Parser B.Tag
firstTag = tag

-- | Tags must be separated by at least one space. (Is this a good
-- restriction? Does not seem to be necessary.)
nextTag :: Parser B.Tag
nextTag =
  char ' '
  >> skipMany (char ' ')
  >> tag

tags :: Parser B.Tags
tags = do
  f <- firstTag
  rs <- many (try nextTag)
  return $ B.Tags (f : rs)


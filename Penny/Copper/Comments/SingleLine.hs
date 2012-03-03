module Penny.Copper.Comments.SingleLine where

import qualified Data.Char as C
import Control.Monad ( void )
import Data.Ix (range)
import qualified Data.Set as S
import Data.Text ( Text, pack )
import Text.Parsec (
  try, many, char, satisfy, string )
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat)

data Comment = Comment Text
               deriving Show

isSingleLineChar :: Char -> Bool
isSingleLineChar c = inCategory || allowed where
  allowed = c `elem` " "
  inCategory = inCat C.UppercaseLetter C.OtherSymbol c

comment :: Parser Comment
comment = do
  void $ try (string "//")
  cs <- many (satisfy (/= '\n'))
  void $ char '\n'
  return (Comment (pack cs))

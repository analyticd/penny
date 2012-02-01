module Penny.Parser.Comments.SingleLine where

import Control.Monad ( void )
import Data.Text ( Text, pack )
import Text.Parsec (
  try, many, char, satisfy, string )
import Text.Parsec.Text ( Parser )

data Comment = Comment Text
               deriving Show

comment :: Parser Comment
comment = do
  void $ try (string "//")
  cs <- many (satisfy (/= '\n'))
  void $ char '\n'
  return (Comment (pack cs))

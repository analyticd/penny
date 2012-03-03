module Penny.Copper.Comments (
  comment, Comment(Single, Multi),
  SingleLine(SingleLine),
  Item(Text, Nested),
  Multiline(Multiline)) where

import Control.Applicative ((<$>), (<*>), (*>), (<|>), pure)
import Control.Monad ( void )
import qualified Data.Char as C
import Data.Ix (range)
import qualified Data.Set as S
import Data.Text ( Text, pack, snoc )
import Text.Parsec (
  try, many, char, satisfy, string, (<?>), anyChar,
  noneOf, notFollowedBy)
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat)
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

data Comment = Single SingleLine
               | Multi Multiline
               deriving Show

data SingleLine = SingleLine Text
                deriving Show

isSingleLineChar :: Char -> Bool
isSingleLineChar c = inCategory || allowed where
  allowed = c `elem` " "
  inCategory = inCat C.UppercaseLetter C.OtherSymbol c

singleLine :: Parser SingleLine
singleLine = do
  _ <- char '/'
  cs <- many (satisfy isSingleLineChar)
  void $ char '\n'
  return (SingleLine (pack cs))

data Item = Text TextNonEmpty
            | Nested Comment
            deriving Show

data Multiline = Multiline [Item]
                 deriving Show

comment :: Parser Comment
comment = p <?> "comment" where
  p = char '/'
      *> ( (Multi<$> multiline)
           <|> (Single <$> singleLine))
      
star :: Parser Char
star = try (char '*' *> notFollowedBy (char '/') *> pure '*')

multilineText :: Parser Item
multilineText = f <$> valid <*> many valid where
  valid = noneOf "*/" <|> star
  f c cs = Text (TextNonEmpty c (pack cs))

nested :: Parser Item
nested = p <?> "nested comment" where
  p = comment >>= return . Nested

multiline :: Parser Multiline
multiline = do
  _ <- char '*'
  is <- many (multilineText <|> nested)
  _ <- string "*/"
  _ <- char '\n'
  return $ Multiline is

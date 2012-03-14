module Penny.Copper.Comments (
  comment
  , Comment(Single, Multi)
  , SingleLine(SingleLine)
  , Item(Text, Nested)
  , Multiline(Multiline)
  ) where

import Control.Applicative ((<$>), (<*>), (*>), (<|>), (<*), (<$))
import qualified Data.Char as C
import Data.Text ( Text, pack )
import Text.Parsec (
  try, many, char, satisfy, string, (<?>),
  notFollowedBy)
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, eol)
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
singleLine = (SingleLine . pack)
             <$ char '/'
             <*> many (satisfy isSingleLineChar)
             <?> "single line comment"

data Item = Text TextNonEmpty
            | Nested Comment
            deriving Show

data Multiline = Multiline [Item]
                 deriving Show

-- | A comment, either single line or multiline, and an EOL with
-- whitespace before and after.
comment :: Parser Comment
comment = char '/'
          *> ( (Multi <$> multiline) <|> (Single <$> singleLine))
          <* eol
          <?> "comment"
      
star :: Parser Char
star = try (char '*' <* notFollowedBy (char '/'))
       <?> "star"

multilineText :: Parser Item
multilineText = f <$> valid <*> many valid where
  valid = satisfy (/= '*') <|> star
  f c cs = Text (TextNonEmpty c (pack cs))

nested :: Parser Item
nested = Nested <$> comment <?> "nested comment"

multiline :: Parser Multiline
multiline = Multiline
            <$ char '*'
            <*> many (multilineText <|> nested)
            <* string "*/"
            <?> "multiline comment"

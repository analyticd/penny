module Penny.Copper.Comments (
  comment
  , Comment(Single, Multi)
  , SingleLine(SingleLine)
  , MultilineItem(MultilineText, Nested)
  , Multiline(Multiline)
  , render
  , isSingleLineChar
  ) where

import Control.Applicative ((<$>), (<*>), (*>), (<|>), (<*), (<$))
import qualified Data.Char as C
import Data.Text ( Text, pack, append, cons, snoc )
import qualified Data.Text as X
import Text.Parsec (
  try, many, many1, char, satisfy, string, (<?>),
  notFollowedBy)
import Text.Parsec.Text ( Parser )

import Penny.Copper.Util (inCat, eol)

data Comment = Single SingleLine
               | Multi Multiline
               deriving (Eq, Show)

data SingleLine = SingleLine Text
                deriving (Eq, Show)

isSingleLineChar :: Char -> Bool
isSingleLineChar c = inCategory || allowed where
  allowed = c `elem` " "
  inCategory = inCat C.UppercaseLetter C.OtherSymbol c

singleLine :: Parser SingleLine
singleLine = (SingleLine . pack)
             <$ char '/'
             <*> many1 (satisfy isSingleLineChar)
             <?> "single line comment"

data MultilineItem = MultilineText Text
            | Nested Comment
            deriving (Eq, Show)

data Multiline = Multiline [MultilineItem]
                 deriving (Eq, Show)

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

multilineText :: Parser MultilineItem
multilineText = (MultilineText . pack) <$> many1 valid where
  valid = satisfy (/= '*') <|> star

nested :: Parser MultilineItem
nested = Nested <$> comment <?> "nested comment"

multiline :: Parser Multiline
multiline = Multiline
            <$ char '*'
            <*> many (multilineText <|> nested)
            <* string "*/"
            <?> "multiline comment"

-- | Render a multiline item. Must not be null, because you cannot
-- parse an empty multiline (Parsec will throw an exception if you
-- apply 'many' to a parser that takes an empty string).
renderMultilineItem :: MultilineItem -> Maybe Text
renderMultilineItem m = case m of
  MultilineText t ->
    if not (null (X.breakOnAll (pack "*/") t)) || X.null t
    then Nothing
    else Just t
  Nested c -> render c

-- | Render a single line item. Must not be null, for identical reason
-- to 'renderMultilineItem'.
renderSingleLine :: SingleLine -> Maybe Text
renderSingleLine (SingleLine s) =
  case X.find (not . isSingleLineChar) s of
    Nothing -> if X.null s then Nothing else Just $ '/' `cons` s
    Just _ -> Nothing

renderMultiline :: Multiline -> Maybe Text
renderMultiline (Multiline is) =
  (\t -> '*' `cons` t `append` (pack "*/"))
  <$> (X.concat
       <$> mapM renderMultilineItem is)

-- | Renders a comment. Fails if any of the characters are banned.
render :: Comment -> Maybe Text
render c =
  (\t -> '/' `cons` t `snoc` '\n')
  <$> (case c of
          Single s -> renderSingleLine s
          Multi m -> renderMultiline m)

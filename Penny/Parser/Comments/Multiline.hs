module Penny.Parser.Comments.Multiline where

import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

import Control.Monad ( void )
import Data.Text ( pack )
import Text.Parsec ( noneOf, try, char, notFollowedBy,
                     (<|>), many, string )
import Text.Parsec.Text ( Parser )

data Item = Text TextNonEmpty
            | Nested Multiline
            deriving Show

data Multiline = Multiline [Item]
                 deriving Show

text :: Parser Item
text = let
  mostChar = noneOf "{-"
  brace = try $ do
    void $ char '{'
    notFollowedBy (char '-')
    return '{'
  dash = try $ do
    void $ char '-'
    notFollowedBy (char '}')
    return '-'
  in do
    c <- mostChar <|> brace <|> dash
    cs <- many (mostChar <|> brace <|> dash)
    return (Text (TextNonEmpty c (pack cs)))

multiline :: Parser Multiline
multiline = do
  void $ string "{-"
  is <- many (text <|> nested)
  void $ string "-}"
  void $ char '\n'
  return $ Multiline is

nested :: Parser Item
nested = multiline >>= return . Nested


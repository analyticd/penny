module Penny.Copper.Util where

import Control.Applicative ((<*), pure, (<$))
import qualified Data.Text as X
import Text.Parsec (char, many, skipMany)
import Text.Parsec.Text (Parser)

unicodeAll :: Char -> Bool
unicodeAll c =
  c > '\x7f' && (c < '\xD800' || c > '\xDFFF')

letter :: Char -> Bool
letter c =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')

digit :: Char -> Bool
digit c = c >= '0' && c <= '9'

asciiAll :: Char -> Bool
asciiAll c = c >= ' ' && c < '\x7f'

-- | Creates a new parser that behaves like the old one, but also
-- parses any whitespace remaining afterward.
lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany (char ' ')

-- | Parses any trailing whitespace followed by a newline followed by
-- additional whitespace.
eol :: Parser ()
eol = pure ()
      <* skipMany (char ' ')
      <* char '\n'
      <* skipMany (char ' ')

-- | Parses a run of spaces.
spaces :: Parser ()
spaces = () <$ many (char ' ')

-- | Takes a field that may or may not be present and a function that
-- renders it. If the field is not present at all, returns an empty
-- Text. Otherwise will succeed or fail depending upon whether the
-- rendering function succeeds or fails.
renMaybe :: Maybe a -> (a -> Maybe X.Text) -> Maybe X.Text
renMaybe mx f = case mx of
  Nothing -> Just X.empty
  Just a -> f a

-- | Merges a list of words into one Text; however, if any given Text
-- is empty, that Text is first dropped from the list.
txtWords :: [X.Text] -> X.Text
txtWords xs = case filter (not . X.null) xs of
  [] -> X.empty
  rs -> X.unwords rs

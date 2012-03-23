-- | WARNING! These parsers were designed to work with a file written
-- entirely in US-ASCII! I don't know how well they will work on a
-- file that has other characters in it.
module Penny.Denver.Parser where

import Control.Applicative(
  (<$>), (<*>), (<*), (*>), (<$), many, pure,
  (<|>))
import Control.Monad (replicateM)
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Lincoln as L
import Text.Parsec (char, digit, satisfy, option, optionMaybe)
import qualified Penny.Denver.Posting as P
import Text.Parsec.Text (Parser)

import qualified Penny.Denver.Common as C
import qualified Penny.Denver.TopLine as T

-- | Converts a parser to one that parses itself and then any trailing
-- whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* many (char ' ')

cleared :: Parser C.Cleared
cleared = char '*' *> pure C.Cleared

day :: Parser T.Day
day = do
  y <- read <$> replicateM 4 digit
  _ <- char '-' <|> char '/'
  m <- read <$> replicateM 2 digit
  _ <- char '-' <|> char '/'
  d <- read <$> replicateM 2 digit
  case T.fromGregorianValid y m d of
    Nothing -> fail "invalid day"
    Just dy -> return dy

number :: Parser L.Number
number =
  L.Number
  <$ char '('
  <*> (L.TextNonEmpty
       <$> satisfy (/= ')')
       <*> (X.pack <$> (many (satisfy (/= ')')))))
  <*  char ')'

isPayeeChar :: Char -> Bool
isPayeeChar c = not $ c `elem` "\t\n"

payee :: Parser L.Payee
payee =
  L.Payee
  <$> (L.TextNonEmpty
       <$> satisfy isPayeeChar
       <*> (X.pack <$> many (satisfy isPayeeChar)))

topLine :: Parser T.TopLine
topLine =
  T.TopLine
  <$> lexeme day
  <*> lexeme (option C.NotCleared cleared)
  <*> lexeme (optionMaybe number)
  <*> optionMaybe payee
  <* char '\n'

negative :: Parser P.Sign
negative = char '-' *> pure P.Negative

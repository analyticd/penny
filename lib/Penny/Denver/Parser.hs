-- | WARNING! These parsers were designed to work with a file written
-- entirely in US-ASCII! I don't know how well they will work on a
-- file that has other characters in it.
module Penny.Denver.Parser where

import Control.Applicative(
  (<$>), (<*>), (<*), (*>), (<$), many, pure,
  (<|>))
import Control.Monad (replicateM)
import qualified Data.Char as Char
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Lincoln as L
import Text.Parsec (char, digit, satisfy, option, optionMaybe,
                    letter, alphaNum)
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

isCurrencySymbol :: Char -> Bool
isCurrencySymbol c = Char.generalCategory c == Char.CurrencySymbol

commodity :: Parser P.Commodity
commodity = currency <|> named where
  currency = P.Commodity
             <$> (L.TextNonEmpty
                  <$> satisfy isCurrencySymbol
                  <*> pure X.empty)
  named = P.Commodity
          <$> (L.TextNonEmpty
               <$> letter
               <*> (X.pack <$> many alphaNum))

{-
Possible combinations for entries. Things in brackets are optional.
C means Commodity, Q means Qty, N means Negative sign, P means Space.
Spaces here are for readability only and do not have meaning.

Commodity on left side:
[N] C [P] [N] Q

Commodity on right side:
[N] Q [P] C

-}

commodityOnLeft ::
  Bool -- ^ Negative sign?
  -> P.Commodity
  -> Bool -- ^ Space?
  -> Bool -- ^ Negative sign?
  -> L.Qty
  -> Maybe P.Entry
commodityOnLeft n1 c s n2 q = let
  sign = case (n1, n2) of
    (True, True) -> Nothing
    (True, False) -> Just P.Negative
    (False, True) -> Just P.Negative
    (False, False) -> Just P.Positive
  fmt = L.Format L.CommodityOnLeft btwn
  btwn = if s then L.SpaceBetween else L.NoSpaceBetween
  amt = P.Amount q c
  in case sign of
    Nothing -> Nothing
    Just sgn -> Just $ P.Entry sgn amt fmt

commodityOnRight ::
  Bool -- ^ Negative?
  -> L.Qty
  -> Bool -- ^ Space?
  -> P.Commodity
  -> P.Entry
commodityOnRight n q s c = P.Entry sgn amt fmt where
  sgn = if n then P.Negative else P.Positive
  amt = P.Amount q c
  fmt = L.Format L.CommodityOnRight bet
  bet = if s then L.SpaceBetween else L.NoSpaceBetween

parseCtyOnLeft :: Parser P.Entry
parseCtyOnLeft = let
  r = commodityOnLeft
      <$> option False (negative *> True)
      <*> 
      

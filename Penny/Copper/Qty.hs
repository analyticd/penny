-- | Quantity parsers. There are two kinds of quantity parsers:
--
-- * Level 1 quantity. Can include spaces, if the grouping character
-- is a space. However these must be quoted when in a Ledger file
-- (from the command line they need not be quoted). The quote
-- character is a caret, @^@.
--
-- * Level 2 quantity. These include no spaces. They need not be
-- quoted.
--
-- A BNF specification for quantities appears in the source code
-- comments.
module Penny.Copper.Qty where

import Control.Applicative ((<$>), (<*>), (<$), (*>), optional)
import qualified Data.List.NonEmpty as NE
import Text.Parsec ( char, (<|>), many, many1, try, (<?>), 
                     sepBy1)
import qualified Text.Parsec as P
import Text.Parsec.Text ( Parser )

import Penny.Lincoln.Bits.Qty ( Qty, partialNewQty )

data Radix = RComma | RPeriod deriving Show
data Grouper = GComma | GPeriod | GSpace deriving Show

data RadGroup = RadGroup Radix Grouper deriving Show

-- | Radix is period, grouping is comma
periodComma :: RadGroup
periodComma = RadGroup RPeriod GComma

-- | Radix is period, grouping is space
periodSpace :: RadGroup
periodSpace = RadGroup RPeriod GSpace

-- | Radix is comma, grouping is period
commaPeriod :: RadGroup
commaPeriod = RadGroup RComma GPeriod

-- | Radix is comma, grouping is space
commaSpace :: RadGroup
commaSpace = RadGroup RComma GSpace

parseRadix :: Radix -> Parser ()
parseRadix r = () <$ char c <?> "radix point" where
  c = case r of RComma -> ','; RPeriod -> '.'

parseGrouper :: Grouper -> Parser ()
parseGrouper g = () <$ char c <?> "grouping character" where
  c = case g of
    GComma -> ','
    GPeriod -> '.'
    GSpace -> ' '

{- a BNF style specification for numbers.

<radix> ::= (as specified)
<grouper> ::= (as specified)
<digit> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"
<digits> ::= <digit> | <digit> <digits>
<firstGroups> ::= <digits> <grouper> <digits>
<nextGroup> ::= <grouper> <digits>
<nextGroups> ::= <nextGroup> | <nextGroup> <nextGroups>
<allGroups> ::= <firstGroups> | <firstGroups> <nextGroups>
<whole> ::= <allGroups> | <digits>
<fractional> ::= <digits>
<number> ::= <whole> <radix>
             | <radix> <fractional>
             | <whole> <radix> <fractional>
-}

newtype Digit = Digit Char
digit :: Parser Digit
digit = Digit <$> P.digit <?> "digit"

data Digits = Digits (NE.NonEmpty Digit)
digits :: Parser Digits
digits = Digits <$> parseNonEmpty digit <?> "digits"

data FirstGroups = FirstGroups Digits Digits
firstGroups :: Grouper -> Parser FirstGroups
firstGroups g = let
  f d1 _ d2 = FirstGroups d1 d2
  e = "first group of digits" in
  f <$> digits <*> parseGrouper g <*> digits <?> e

newtype NextGroup = NextGroup Digits
nextGroup :: Grouper -> Parser NextGroup
nextGroup g = parseGrouper g *> (NextGroup <$> digits)

newtype NextGroups = NextGroups (NE.NonEmpty NextGroup)
nextGroups :: Grouper -> Parser NextGroups
nextGroups g = NextGroups <$> parseNonEmpty (nextGroup g)

data AllGroups = FirstOnly FirstGroups
                 | FirstAndNext FirstGroups NextGroups
allGroups :: Grouper -> Parser AllGroups
allGroups g = f <$> firstGroups g <*> optional (nextGroups g) where
  f fg ng = case ng of
    Nothing -> FirstOnly fg
    Just n -> FirstAndNext fg n

firstOrDigits :: Grouper -> Parser (Either FirstGroups Digits)
firstOrDigits g = f <$> digits <*> optional (nextGroup g) <?> e where
  e = "first group of digits"
  f g1 g2 = case g2 of
    Nothing -> Right g1
    Just (NextGroup d) -> Left $ FirstGroups g1 d

data Whole = Whole (Either AllGroups Digits)
whole :: Grouper -> Parser Whole
whole g = f <$> firstOrDigits g <*> (optional (nextGroups g)) where
  f fd mayNg = Whole $ case fd of
    Left fg -> Left $ case mayNg of
      Nothing -> FirstOnly fg
      Just ng -> FirstAndNext fg ng
    Right d -> Right d

shorter :: Grouper -> Parser String
shorter g = concat
            <$> sepBy1 (many1 P.digit) (parseGrouper g)

parseNonEmpty :: Parser a -> Parser (NE.NonEmpty a)
parseNonEmpty p = NE.nonEmpty <$> p <*> many p



lvl1QtyUnquoted :: Parser Qty
lvl1QtyUnquoted = undefined

{-
newtype Radix = Radix { unRadix :: Char }
newtype Separator = Separator { unSeparator :: Char }

radixAndSeparator :: Char -> Char -> (Radix, Separator)
radixAndSeparator r s =
  if r == s
  then error "radix and separator must be different characters"
  else (Radix r, Separator s)

radix :: Radix -> Parser Char
radix (Radix r) = char r >> return '.'

qtyDigit :: Separator -> Parser Char
qtyDigit (Separator separator) = digit <|> (char separator >> digit)

qty :: Radix -> Separator -> Parser Qty
qty rdx sep = let
  digitRun = do
    c <- digit
    cs <- many (qtyDigit sep)
    return (c : cs)
  withPoint = do
    l <- digitRun
    p <- radix rdx
    r <- digitRun
    return (l ++ (p : r))
  withoutPoint = digitRun
  in do
    s <- try withPoint <|> withoutPoint
    let d = read s
    return $ partialNewQty d

-}

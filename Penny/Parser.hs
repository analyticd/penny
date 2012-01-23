module Penny.Parser where

import Text.Parsec
import Text.Parsec.Text

import Data.Char ( isLetter, isNumber, isPunctuation, isSymbol )
import qualified Data.Char as Char
import Control.Monad ( void, liftM )
import Data.Text ( pack, empty )

import qualified Penny.Posting as P
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )
import Penny.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ) )

multiline :: Parser ()
multiline = let
  inner = try multiline <|> void anyChar
  in string "{-"
     >> manyTill inner (try (string "-}"))
     >> return ()

subAccountChar :: Parser Char
subAccountChar = let
  notSpc = satisfy (\l -> isLetter l || isNumber l)
  spc = do
    void $ char ' '
    notFollowedBy (char ' ' <|> char '#')
    return ' '
  in notSpc <|> try spc

subAccountName :: Parser P.SubAccountName
subAccountName = do
  c <- subAccountChar
  r <- liftM pack $ many subAccountChar
  return . P.SubAccountName $ TextNonEmpty c r

firstSubAccount :: Parser P.SubAccountName
firstSubAccount = subAccountName

nextSubAccount :: Parser P.SubAccountName
nextSubAccount = char ':' >> subAccountName

account :: Parser P.Account
account = do
  f <- firstSubAccount
  r <- many nextSubAccount
  return . P.Account $ AtLeast1 f r

tagChar :: Parser Char
tagChar = satisfy (\l -> isLetter l || isNumber l)

tag :: Parser P.TagName
tag = do
  _ <- char '#'
  f <- tagChar
  r <- liftM pack (many tagChar)
  return . P.TagName $ TextNonEmpty f r

firstTag :: Parser P.TagName
firstTag = tag

-- Do not use Parsec's "space" parser for spaces. It parses any
-- whitespace character, including tabs and newlines. Instead use char
-- ' '.

-- | Tags must be separated by at least one space. (Is this a good
-- restriction? Does not seem to be necessary.)
nextTag :: Parser P.TagName
nextTag =
  skipMany multiline
  >> char ' '
  >> skipMany (multiline <|> void (char ' '))
  >> tag

tags :: Parser P.Tags
tags = do
  f <- firstTag
  rs <- many (try nextTag)
  return $ P.Tags (f : rs)

drCr :: Parser P.DrCr
drCr = let
  dr = do
    void (char 'D')
    void (char 'r') <|> void (string "ebit")
    return P.Debit
  cr = do
    void (string "Cr")
    void (optional (string "edit"))
    return P.Credit
  in dr <|> cr

commoditySymbol :: Parser P.Commodity
commoditySymbol = do
  let p ch = Char.generalCategory ch == Char.CurrencySymbol
  c <- satisfy p
  return . P.Commodity $ TextNonEmpty c empty

commodityLong :: Parser P.Commodity
commodityLong = do
  c <- satisfy isLetter
  rs <- many $ satisfy (\l -> isLetter l || isNumber l)
  return . P.Commodity $ TextNonEmpty c (pack rs)

priceDesc :: Parser P.PriceDesc
priceDesc = do
  void $ char '@'
  option P.UnitPrice (char '@' >> return P.TotalPrice)

{-
payeeChar :: Parser Char
payeeChar = let
  notSpc = 
-}

payeeChar :: Parser Char
payeeChar = let
  spc = do
    void $ char ' '
    notFollowedBy (void (char ' ') <|> (void (try (string "--")))
                   <|> (void (try (string "{-"))))
    return ' '
  p c = isLetter c || isNumber c
        || isPunctuation c || isSymbol c
  in satisfy p <|> try spc

payee :: Parser P.Payee
payee = do
  c <- payeeChar
  rs <- liftM pack (many payeeChar)
  return . P.Payee $ TextNonEmpty c rs


{-
isPayeeChar :: Char -> Bool
isPayeeChar c = not (banned c) && allowed c where
  banned c = c `elem` bads
  bads = ['\t', '\n', '\r', '\f', '\v', '{', '}']
-}

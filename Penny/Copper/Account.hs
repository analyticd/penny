-- | Account parsers. Account names fall into one of three groups:
--
-- * Level 1 account. Can have nearly any character, including
-- spaces. However, when in a Ledger file they must be quoted.
--
-- * Level 2 account. The first sub-account begins with a letter. All
-- other characters may be nearly any character, except for a space.
module Penny.Copper.Account where

import Control.Applicative((<$>), (<*>), pure)
import Control.Monad ( liftM, void )
import Data.Char ( isLetter, isNumber )
import qualified Data.Char as C
import Data.Text ( pack )
import Text.Parsec (
  char, satisfy, notFollowedBy, (<|>), try, many, (<?>),
  many1, between, sepBy1 )
import Text.Parsec.Text ( Parser )

import Data.List.NonEmpty (nonEmpty, unsafeToNonEmpty)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ),
                                    unsafeTextNonEmpty )
import Penny.Copper.Util (inCat)

lvl1Char :: Char -> Bool
lvl1Char c = allowed && notBanned where
  allowed = inCat C.UppercaseLetter C.OtherSymbol c
  notBanned = not $ c `elem` "}:"

lvl1Sub :: Parser B.SubAccountName
lvl1Sub = f <$> p <?> e where
  f = B.SubAccountName . unsafeTextNonEmpty
  p = many1 (satisfy lvl1Char)
  e = "sub account name"

lvl1Account :: Parser B.Account
lvl1Account = B.Account . unsafeToNonEmpty <$> p <?> e where
  e = "account name"
  p = sepBy1 lvl1Sub (char ':')

lvl1AccountQuoted :: Parser B.Account
lvl1AccountQuoted = between (char '{') (char '}') lvl1Account

lvl2AccountFirst :: Parser B.SubAccountName
lvl2AccountFirst = f <$> c1 <*> cs <?> e where
  c1 = satisfy (inCat C.UppercaseLetter C.OtherLetter)
  p c = allowed && notBanned where
    allowed = inCat C.UppercaseLetter C.OtherSymbol c || c == ' '
    notBanned = not $ c `elem` "}:"
  cs = many (satisfy p)
  f l1 lr = B.SubAccountName (TextNonEmpty l1 (pack lr))
  e = "account name beginning with a letter"
  

subAccountChar :: Parser Char
subAccountChar = let
  notSpc = satisfy (\l -> isLetter l || isNumber l)
  spc = do
    void $ char ' '
    notFollowedBy (char ' ' <|> char '#')
    return ' '
  in notSpc <|> try spc

subAccountName :: Parser B.SubAccountName
subAccountName = do
  c <- subAccountChar
  r <- liftM pack $ many subAccountChar
  return . B.SubAccountName $ TextNonEmpty c r

firstSubAccount :: Parser B.SubAccountName
firstSubAccount = subAccountName

nextSubAccount :: Parser B.SubAccountName
nextSubAccount = char ':' >> subAccountName

account :: Parser B.Account
account = do
  f <- firstSubAccount
  r <- many nextSubAccount
  return . B.Account $ nonEmpty f r


-- | Account parsers. Account names fall into one of three groups:
--
-- * Level 1 account. Can have nearly any character, including
-- spaces. However, when in a Ledger file they must be quoted.
--
-- * Level 2 account. The first sub-account begins with a letter. All
-- other characters may be nearly any character, except for a space.
module Penny.Copper.Account where

import Control.Applicative((<$>), (<*>), (*>), (<$))
import Control.Monad.Exception.Synchronous as Ex
import qualified Data.Char as C
import Data.Text ( pack, Text )
import Text.Parsec (
  char, satisfy, many, (<?>),
  many1, between, sepBy1, option )
import Text.Parsec.Text ( Parser )

import Data.List.NonEmpty (nonEmpty, unsafeToNonEmpty)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ),
                                    unsafeTextNonEmpty )
import qualified Penny.Lincoln.HasText as HT
import Penny.Copper.Util (inCat)
import qualified Penny.Copper.Util as U

lvl1Char :: Char -> Bool
lvl1Char c = allowed && notBanned where
  allowed = inCat C.UppercaseLetter C.OtherSymbol c || c == ' '
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

lvl2SubAccountFirst :: Parser B.SubAccountName
lvl2SubAccountFirst = f <$> c1 <*> cs <?> e where
  c1 = satisfy (inCat C.UppercaseLetter C.OtherLetter)
  p c = allowed && notBanned where
    allowed = inCat C.UppercaseLetter C.OtherSymbol c || c == ' '
    notBanned = not $ c `elem` "}:"
  cs = many (satisfy p)
  f l1 lr = B.SubAccountName (TextNonEmpty l1 (pack lr))
  e = "sub account name beginning with a letter"
  
lvl2SubAccountRest :: Parser B.SubAccountName
lvl2SubAccountRest = f <$> cs <?> e where
  cs = many1 (satisfy p)
  p c = allowed && notBanned where
    allowed = inCat C.UppercaseLetter C.OtherSymbol c
    notBanned = not $ c `elem` "}:"
  f = B.SubAccountName . unsafeTextNonEmpty
  e = "sub account name"

lvl2Account :: Parser B.Account
lvl2Account = f <$> p1 <*> p2 <?> e where
  f x y = B.Account (nonEmpty x y)
  p1 = lvl2SubAccountFirst
  p2 = option [] $
       char ':' *> sepBy1 lvl2SubAccountRest (char ':')
  e = "account name"

renderSubAccount ::
  B.SubAccountName
  -> Ex.Exceptional U.RenderError Text
renderSubAccount = undefined

-- | Account parsers. Account names fall into one of three groups:
--
-- * Level 1 account. Can have nearly any character, including
-- spaces. However, when in a Ledger file they must be quoted.
--
-- * Level 2 account. The first sub-account begins with a letter. All
-- other characters may be nearly any character, except for a space.
module Penny.Copper.Account
  ( lvl1Account
  , lvl1AccountQuoted
  , lvl2Account
  , render
  , lvl1Char
  , lvl2FirstChar
  , lvl2RemainingChar
  ) where

import Control.Applicative((<$>), (<*>), (*>), (<|>))
import Control.Monad (guard)
import Data.List (intersperse)
import Data.Text ( pack, Text )
import qualified Data.Text as X
import Text.Parsec (
  char, satisfy, many, (<?>),
  many1, between, sepBy1, option )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Util as U

-- | Characters allowed in a Level 1 account. (Check the source code
-- to see what these are).
lvl1Char :: Char -> Bool
lvl1Char c = allowed && not banned where
  allowed = U.unicodeAll c || U.asciiAll c
  banned = (c == '}') || (c == ':')

lvl1Sub :: Parser B.SubAccount
lvl1Sub = f <$> p <?> e where
  f = B.SubAccount . pack
  p = many1 (satisfy lvl1Char)
  e = "sub account name"

-- | Meant to be used when parsing values from the command line. Do
-- not use this function when parsing values from the ledger file. It
-- will fail. Instead use 'lvl1AccountQuoted'. Rendered accounts may
-- be quoted, so this function may fail if applied to rendered
-- accounts.
lvl1Account :: Parser B.Account
lvl1Account = B.Account <$> p <?> e where
  e = "account name"
  p = sepBy1 lvl1Sub (char ':')

lvl1AccountQuoted :: Parser B.Account
lvl1AccountQuoted = between (char '{') (char '}') lvl1Account

-- | Characters allowed for the first character of a Level 2 account.
lvl2FirstChar :: Char -> Bool
lvl2FirstChar c = U.unicodeAll c || U.letter c

-- | Characters allowed for the remaining characters of a Level 2
-- account.
lvl2RemainingChar :: Char -> Bool
lvl2RemainingChar c = allowed && not banned where
    allowed = U.unicodeAll c || U.asciiAll c
    banned = c == ':'

lvl2SubAccountFirst :: Parser B.SubAccount
lvl2SubAccountFirst = f <$> c1 <*> cs <?> e where
  c1 = satisfy lvl2FirstChar
  cs = many (satisfy lvl2RemainingChar)
  f l1 lr = B.SubAccount (pack (l1:lr))
  e = "sub account name beginning with a letter"

lvl2SubAccountRest :: Parser B.SubAccount
lvl2SubAccountRest = f <$> cs <?> e where
  cs = many1 (satisfy p)
  p c = allowed && not banned where
    allowed = U.unicodeAll c || U.asciiAll c
    banned = c == ':'
  f = B.SubAccount . pack
  e = "sub account name"

lvl2Account :: Parser B.Account
lvl2Account = f <$> p1 <*> p2 <?> e where
  f x y = B.Account (x : y)
  p1 = lvl2SubAccountFirst
  p2 = option [] $
       char ':' *> sepBy1 lvl2SubAccountRest (char ':')
  e = "account name"





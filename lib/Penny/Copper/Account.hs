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

import Control.Applicative((<$>), (<*>), (*>))
import Data.List (intersperse)
import qualified Data.Foldable as F
import qualified Data.Monoid as M
import Data.Text ( snoc, cons, pack, Text )
import qualified Data.Text as X
import qualified Data.Traversable as T
import Text.Parsec (
  char, satisfy, many, (<?>),
  many1, between, sepBy1, option )
import Text.Parsec.Text ( Parser )

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ),
                                    unsafeTextNonEmpty )
import qualified Penny.Lincoln.HasText as HT
import qualified Penny.Copper.Util as U

-- | Characters allowed in a Level 1 account. (Check the source code
-- to see what these are).
lvl1Char :: Char -> Bool
lvl1Char c = allowed && not banned where
  allowed = U.unicodeAll c || U.asciiAll c
  banned = (c == '{') || (c == ':')

lvl1Sub :: Parser B.SubAccount
lvl1Sub = f <$> p <?> e where
  f = B.SubAccount . pack
  p = many1 (satisfy lvl1Char)
  e = "sub account name"

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

data Level = L1 | L2
           deriving (Eq, Ord, Show)

-- | Is True if a sub account can be rendered at Level 1;
-- False otherwise.
isSubAcctLvl1 :: B.SubAccount -> Bool
isSubAcctLvl1 (B.SubAccount x) =
  (not . X.null $ x)
  && (X.all lvl1Char x)

isAcctLvl1 :: B.Account -> Bool
isAcctLvl1 (B.Account ls) =
  (not . null $ ls)
  && (all isSubAcctLvl1 ls)

firstSubAcctLvl2 :: B.SubAccount -> Bool
firstSubAcctLvl2 (B.SubAccount x) = case X.uncons x of
  Nothing -> False
  Just (c, r) -> lvl2FirstChar c && (X.all lvl2RemainingChar r)

otherSubAcctLvl2 :: B.SubAccount -> Bool
otherSubAcctLvl2 (B.SubAccount x) =
  (not . X.null $ x)
  && (X.all lvl2RemainingChar x)

isAcctLvl2 :: B.Account -> Bool
isAcctLvl2 (B.Account ls) = case ls of
  [] -> False
  x:xs -> firstSubAcctLvl2 x && all otherSubAcctLvl2 xs

acctLvl :: B.Account -> Maybe Level
acctLvl a = M.getFirst . M.mconcat . map M.First $
  [ if isAcctLvl1 a then Just L1 else Nothing
  , if isAcctLvl2 a then Just L2 else Nothing
  ]

-- | Shows an account, with the minimum level of quoting
-- possible. Fails with an error if any one of the characters in the
-- account name does not satisfy the 'lvl1Char' predicate. Otherwise
-- returns a rendered account, quoted if necessary.
render :: B.Account -> Maybe Text
render a = fmap toTxt (acctLvl a)
  where
    t = X.concat . intersperse (X.singleton ':')
        . map B.unSubAccount . B.unAccount $ a
    toTxt l = case l of
      L1 -> cons '{' t `snoc` '}'
      L2 -> t

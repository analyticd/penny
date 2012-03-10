-- | Account parsers. Account names fall into one of three groups:
--
-- * Level 1 account. Can have nearly any character, including
-- spaces. However, when in a Ledger file they must be quoted.
--
-- * Level 2 account. The first sub-account begins with a letter. All
-- other characters may be nearly any character, except for a space.
module Penny.Copper.Account (
  lvl1Account
  , lvl1AccountQuoted
  , lvl2Account
  , render
  , lvl1Char
  , lvl2FirstChar
  , lvl2RemainingChar
  ) where

import Control.Applicative((<$>), (<*>), (*>))
import Control.Monad.Exception.Synchronous as Ex
import qualified Data.Char as C
import qualified Data.Foldable as F
import Data.Text ( snoc, cons, pack, Text )
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
import Penny.Copper.Util (inCat)
import qualified Penny.Copper.Util as U

-- | Characters allowed in a Level 1 account. (Check the source code
-- to see what these are).
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
lvl1Account = B.Account . NE.fromList <$> p <?> e where
  e = "account name"
  p = sepBy1 lvl1Sub (char ':')

lvl1AccountQuoted :: Parser B.Account
lvl1AccountQuoted = between (char '{') (char '}') lvl1Account

-- | Characters allowed for the first character of a Level 2 account.
lvl2FirstChar :: Char -> Bool
lvl2FirstChar = inCat C.UppercaseLetter C.OtherLetter

-- | Characters allowed for the remaining characters of a Level 2
-- account.
lvl2RemainingChar :: Char -> Bool
lvl2RemainingChar c = allowed && notBanned where
    allowed = inCat C.UppercaseLetter C.OtherSymbol c
    notBanned = not $ c `elem` "}:"

lvl2SubAccountFirst :: Parser B.SubAccountName
lvl2SubAccountFirst = f <$> c1 <*> cs <?> e where
  c1 = satisfy lvl2FirstChar
  cs = many (satisfy lvl2RemainingChar)
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
  f x y = B.Account (x :| y)
  p1 = lvl2SubAccountFirst
  p2 = option [] $
       char ':' *> sepBy1 lvl2SubAccountRest (char ':')
  e = "account name"

data Level = L1 | L2
           deriving (Eq, Ord, Show)

-- | Checks an account to see what level to render it at.
checkAccount :: B.Account -> Ex.Exceptional U.RenderError Level
checkAccount (B.Account subs) = let
  checkFirst = checkFirstSubAccount (NE.head subs)
  checkRest = map checkFirstSubAccount (NE.tail subs)
  in F.minimum <$> T.sequenceA (checkFirst : checkRest)

-- | Checks the first sub account to see if it qualifies as a Level 1
-- or Level 2 sub account.
checkFirstSubAccount ::
  B.SubAccountName
  -> Ex.Exceptional U.RenderError Level
checkFirstSubAccount s = do
  l <- checkOtherSubAccount s
  return $ case l of
    L1 -> L1
    L2 -> let (B.SubAccountName (TextNonEmpty c _)) = s
          in if lvl2FirstChar c then L2 else L1

-- | Checks a sub account other than the first one to see if it
-- qualifies as a Level 1 or Level 2 sub account.
checkOtherSubAccount ::
  B.SubAccountName
  -> Ex.Exceptional U.RenderError Level
checkOtherSubAccount = U.checkText ls where
  ls = (lvl2RemainingChar, L2) :| [(lvl1Char, L1)]

-- | Shows an account, with the minimum level of quoting
-- possible. Fails with an error if any one of the characters in the
-- account name does not satisfy the 'lvl1Char' predicate. Otherwise
-- returns a rendered account, quoted if necessary.
render :: B.Account -> Ex.Exceptional U.RenderError Text
render a = do
  l <- checkAccount a
  let t = HT.text . HT.Delimited (pack ":") . HT.textList $ a
  return $ case l of
    L1 -> cons '{' t `snoc` '}'
    L2 -> t

-- | Commodity parsers. Commodity names fall into three groups:
--
-- * Level 1 commodities. These can have a broad selection of
-- characters, including spaces. The downside is that they need to be
-- quoted when they appear in a file. (They don't have to be quoted
-- from the command line, presuming that a single command line
-- argument captures the entire commodity name and nothing else.)
--
-- * Level 2 commodities. The first sub-commodity begins with a letter
-- or a symbol. All other characters may be nearly any other
-- character. Spaces however are not permitted.
--
-- * Level 3 commodities. All charcters must be letters or symbols.
module Penny.Copper.Commodity where

import Control.Applicative ((<*>), (<$>), (*>), (<|>))
import qualified Data.Char as C
import Data.Text ( pack )
import Text.Parsec ( satisfy, many, char, sepBy1, many1, (<?>),
                     between, option )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Penny.Copper.Util (inCat)
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ),
                                    unsafeTextNonEmpty )

-- | Most liberal set of letters allowed in a commodity. 
lvl1Char :: Char -> Bool
lvl1Char c = (category || specific) && notBanned where
  category = inCat C.UppercaseLetter C.OtherSymbol c
  specific = c == ' '
  notBanned = not $ c `elem` ['"', ':']

-- | A sub commodity comprised of the most liberal characters.
lvl1SubCmdty :: Parser B.SubCommodity
lvl1SubCmdty = f <$> m <?> "sub commodity" where
  m = many1 (satisfy lvl1Char)
  f cs = B.SubCommodity (TextNonEmpty (head cs) (pack $ tail cs))

-- | A commodity that might have spaces inside of the name. To parse
-- this when it is in a ledger file, it must be quoted; use
-- quotedLvl1Cmdty for that. This parser can be used directly for values
-- entered from the command line.
lvl1Cmdty :: Parser B.Commodity
lvl1Cmdty = (B.Commodity . fromList)
                  <$> sepBy1 lvl1SubCmdty (char ':')
                  <?> "commodity with spaces"


-- | A commodity that may have spaces in the name; is wrapped inside
-- of double quotes.
quotedLvl1Cmdty :: Parser B.Commodity
quotedLvl1Cmdty = between q q lvl1Cmdty
              <?> "quoted commodity" where
                q = char '"'

-- | Allows only letters and symbols.
lvl2FirstChar :: Char -> Bool
lvl2FirstChar c = inCat C.UppercaseLetter C.OtherLetter c
               || inCat C.MathSymbol C.CurrencySymbol c

lvl2OtherChars :: Char -> Bool
lvl2OtherChars c = category && notBanned where
  category = inCat C.UppercaseLetter C.OtherSymbol c
  notBanned = not $ c `elem` ['"', ':']

lvl2FirstSubCmdty :: Parser B.SubCommodity
lvl2FirstSubCmdty = f <$> firstLet <*> restLet <?> e where
  e = "sub commodity, first character is letter or symbol"
  firstLet = satisfy lvl2FirstChar
  restLet = many (satisfy lvl2OtherChars)
  f l1 lr = B.SubCommodity (TextNonEmpty l1 (pack lr))

lvl2OtherSubCmdty :: Parser B.SubCommodity
lvl2OtherSubCmdty = f <$> ls <?> e where
  e = "sub commodity"
  ls = many1 (satisfy lvl2OtherChars)
  f = B.SubCommodity . unsafeTextNonEmpty

lvl2Cmdty :: Parser B.Commodity
lvl2Cmdty = f <$> firstSub <*> restSubs <?> e where
  e = "commodity, first character is letter or symbol"
  firstSub = lvl2FirstSubCmdty
  restSubs = option []
             $ char ':'
             *> sepBy1 lvl2OtherSubCmdty (char ':')
  f s1 sr = B.Commodity (s1 :| sr)

lvl3Chars :: Char -> Bool
lvl3Chars c = inCat C.UppercaseLetter C.OtherLetter c
              || inCat C.MathSymbol C.CurrencySymbol c

lvl3SubCmdty :: Parser B.SubCommodity
lvl3SubCmdty = f <$> ls <?> e where 
  e = "sub commodity, letters and symbols only"
  f = B.SubCommodity . unsafeTextNonEmpty
  ls = many1 (satisfy lvl3Chars)

lvl3Cmdty :: Parser B.Commodity
lvl3Cmdty = f <$> ls <?> e where
  f = B.Commodity . fromList
  ls = sepBy1 lvl3SubCmdty (char ':')
  e = "commodity, letters and symbols only"

-- | A commodity being read in from the command line, where the
-- commodity is guaranteed to be the only thing to parse.
commandLineCmdty :: Parser B.Commodity
commandLineCmdty = lvl1Cmdty

-- | A commodity on the left side of a quantity in a ledger file.
leftSideCmdty :: Parser B.Commodity
leftSideCmdty =
  quotedLvl1Cmdty
  <|> lvl3Cmdty
  <?> "commodity to the left of the quantity"

-- | A commodity on the right side of a quantity in a ledger file.
rightSideCmdty :: Parser B.Commodity
rightSideCmdty =
  quotedLvl1Cmdty
  <|> lvl2Cmdty
  <?> "commodity to the right of the quantity"

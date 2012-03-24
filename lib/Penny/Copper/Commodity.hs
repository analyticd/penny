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
-- * Level 3 commodities. All charcters must be letters or symbols. In
-- addition, the first character cannot be a @+@ or a @-@.
module Penny.Copper.Commodity (
  -- * Level 1 commodities
  lvl1Char,
  lvl1Cmdty,
  quotedLvl1Cmdty,
  commandLineCmdty,
  
  -- * Level 2 commodities
  lvl2FirstChar,
  lvl2OtherChars,
  lvl2Cmdty,
  
  -- * Level 3 commodities
  lvl3FirstChar,
  lvl3OtherChars,
  lvl3Cmdty,
  
  -- * Helpers when parsing from a file
  leftSideCmdty,
  rightSideCmdty,
  
  -- * Rendering
  renderQuotedLvl1,
  renderLvl2,
  renderLvl3
  ) where

import Control.Applicative ((<*>), (<$>), (*>), (<|>))
import Control.Monad (guard)
import Data.Text ( pack, Text, cons, snoc, singleton )
import Text.Parsec ( satisfy, many, char, sepBy1, many1, (<?>),
                     between, option, sepBy )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Penny.Copper.Util (listIsOK, firstCharOfListIsOK)
import qualified Penny.Copper.Util as U
import qualified Penny.Lincoln.HasText as HT
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ),
                                    unsafeTextNonEmpty )

-- | Most liberal set of letters allowed in a commodity. 
lvl1Char :: Char -> Bool
lvl1Char c = (category || specific) && notBanned where
  category = U.rangeLettersToSymbols c
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
lvl2FirstChar c = U.rangeLetters c || U.rangeMathCurrency c

lvl2OtherChars :: Char -> Bool
lvl2OtherChars c = category && notBanned where
  category = U.rangeLettersToSymbols c
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

lvl3OtherChars :: Char -> Bool
lvl3OtherChars c = U.rangeLetters c || U.rangeMathCurrency c

lvl3FirstChar :: Char -> Bool
lvl3FirstChar c = lvl3OtherChars c && (not $ c `elem` "+-")

lvl3FirstSubCmdty :: Parser B.SubCommodity
lvl3FirstSubCmdty = f <$> c <*> cs <?> e where
  e = "first sub commodity, letters and symbols only, "
      ++ "first character not a + or -"
  f c1 cr = B.SubCommodity (TextNonEmpty c1 (pack cr))
  c = satisfy lvl3FirstChar
  cs = many (satisfy lvl3OtherChars)

lvl3OtherSubCmdty :: Parser B.SubCommodity
lvl3OtherSubCmdty = f <$> ls <?> e where 
  e = "sub commodity, letters and symbols only"
  f = B.SubCommodity . unsafeTextNonEmpty
  ls = many1 (satisfy lvl3OtherChars)

lvl3Cmdty :: Parser B.Commodity
lvl3Cmdty = f <$> p1 <*> pr <?> e where
  f cf cs = B.Commodity (cf :| cs)
  p1 = lvl3FirstSubCmdty
  pr = option [] $ char ':' *> sepBy lvl3OtherSubCmdty (char ':')
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

-- | Render a quoted Level 1 commodity. Fails if any character does
-- not satisfy lvl1Char.
renderQuotedLvl1 :: B.Commodity -> Maybe Text
renderQuotedLvl1 ca@(B.Commodity c) = do
  guard $ listIsOK lvl1Char ca
  return $ '"'
    `cons` HT.text (HT.Delimited (singleton ':') (HT.textList c))
    `snoc` '"'

-- | Render a Level 2 commodity. Fails if the first character is not a
-- letter or a symbol, or if any other character is a space.
renderLvl2 :: B.Commodity -> Maybe Text
renderLvl2 (B.Commodity c) = do
  guard $ firstCharOfListIsOK lvl2FirstChar c
  guard $ listIsOK lvl2OtherChars c
  return $ HT.text (HT.Delimited (singleton ':') (HT.textList c))

-- | Render a Level 3 commodity. Fails if any character is not a
-- letter or a symbol.
renderLvl3 :: B.Commodity -> Maybe Text
renderLvl3 (B.Commodity c) = do
  guard $ listIsOK lvl3OtherChars c
  guard $ firstCharOfListIsOK lvl3FirstChar c
  return $ HT.text (HT.Delimited (singleton ':') (HT.textList c))

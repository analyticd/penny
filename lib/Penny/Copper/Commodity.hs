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
  lvl3Char,
  lvl3Cmdty,
  
  -- * Helpers when parsing from a file
  leftSideCmdty,
  rightSideCmdty,
  
  -- * Rendering
  renderQuotedLvl1,
  renderLvl2,
  renderLvl3
  ) where

import Control.Applicative ((<*>), (<$>), (<|>))
import Control.Monad (guard)
import Data.Text ( pack, Text, cons, snoc)
import Text.Parsec ( satisfy, many, char, many1, (<?>),
                     between )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Util as U
import qualified Data.Text as X

-- | Most liberal set of letters allowed in a commodity.
lvl1Char :: Char -> Bool
lvl1Char c = allowed && not banned
  where
    allowed = U.rangeAny c
    banned = c == '"'


-- | A commodity that might have spaces inside of the name. To parse
-- this when it is in a ledger file, it must be quoted; use
-- quotedLvl1Cmdty for that. This parser can be used directly for values
-- entered from the command line.
lvl1Cmdty :: Parser B.Commodity
lvl1Cmdty =
  (B.Commodity . X.pack)
  <$> many1 (satisfy lvl1Char)


-- | A commodity that may have spaces in the name; is wrapped inside
-- of double quotes.
quotedLvl1Cmdty :: Parser B.Commodity
quotedLvl1Cmdty = between q q lvl1Cmdty <?> "quoted commodity"
  where
    q = char '"'

-- | Allows only letters and symbols.
lvl2FirstChar :: Char -> Bool
lvl2FirstChar c =
  (c > '\x7F')
  || (c == '$')
  || (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')


lvl2OtherChars :: Char -> Bool
lvl2OtherChars c =
  (c > '\x7F')
  || (c == '$')
  || (c == '_')
  || (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')

lvl2Cmdty :: Parser B.Commodity
lvl2Cmdty = f <$> firstLet <*> restLet <?> e
  where
    e = "commodity, first letter is a letter or symbol"
    firstLet = satisfy lvl2FirstChar
    restLet = many (satisfy lvl2OtherChars)
    f c cs = B.Commodity (pack (c:cs))

lvl3Char :: Char -> Bool
lvl3Char c =
  (c > '\x7F')
  || (c == '$')
  || (c == '_')
  || (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')

lvl3Cmdty :: Parser B.Commodity
lvl3Cmdty = (B.Commodity . X.pack) <$> p <?> e
  where
    p = many1 (satisfy lvl3Char)
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
renderQuotedLvl1 (B.Commodity c) =
  if X.all lvl1Char c
  then Just $ '"' `cons` c `snoc` '"'
  else Nothing


-- | Render a Level 2 commodity. Fails if the first character is not a
-- letter or a symbol, or if any other character is a space.
renderLvl2 :: B.Commodity -> Maybe Text
renderLvl2 (B.Commodity c) = do
  (f, rs) <- X.uncons c
  guard $ lvl2FirstChar f
  guard . X.all lvl2OtherChars $ rs
  return c


-- | Render a Level 3 commodity. Fails if any character is not a
-- letter or a symbol.
renderLvl3 :: B.Commodity -> Maybe Text
renderLvl3 (B.Commodity c) =
  if (not . X.null $ c) && (X.all lvl3Char c)
  then return c
  else Nothing

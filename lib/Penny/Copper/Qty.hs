module Penny.Copper.Qty (

  -- * Rendering
    GroupingSpec(NoGrouping, GroupLarge, GroupAll)
  , render

  -- * Parsing quantities
  , qty
  ) where

import Control.Applicative ((<$>), (<*>), (<$), (*>), optional)
import Data.List (intercalate)
import Data.List.Split (splitEvery, splitOn)
import qualified Data.Text as X
import Text.Parsec ( char, many1, (<?>), digit, many)
import Text.Parsec.Text ( Parser )

import Penny.Lincoln.Bits.Qty ( Qty, newQty, unQty )

{- a BNF style specification for numbers.

<radix> ::= "."
<grouper> ::= "\x2009"
<digit> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0"
<digits> ::= <digit>+
<group> ::= <grouper> <digits>
<sequence> ::= <digits> <group>*
<postSequence> ::= <radix> <sequence>?
<number> ::= <radix> <sequence>
             | <sequence> <postSequence>?
-}

parseRadix :: Parser ()
parseRadix = () <$ char '.' <?> "radix point" where

parseGrouper :: Parser ()
parseGrouper = () <$ char '\x2009' <?> "grouping character, thin space"

parseDigit :: Parser Char
parseDigit = digit

parseDigits :: Parser [Char]
parseDigits = many1 parseDigit

data Group = Group { unGroup :: [Char] }
data Sequence = Sequence [Char] [Group]
data PostSequence = PostSequence (Maybe Sequence)
data Number = NumberRadSeq Sequence
            | NumberSeqPost Sequence (Maybe PostSequence)

parseGroup :: Parser Group
parseGroup = Group <$> (parseGrouper *> parseDigits)

parseSequence :: Parser Sequence
parseSequence = Sequence <$> parseDigits <*> many parseGroup

parsePostSequence :: Parser PostSequence
parsePostSequence = PostSequence <$> optional parseSequence

parseNumber :: Parser Number
parseNumber = f <$> parseSequence <*> optional radPost
  where
    radPost = parseRadix *> optional parsePostSequence
    f sq mayPost = case mayPost of
      Nothing ->  NumberRadSeq sq
      Just post -> NumberSeqPost sq post

sequenceToString :: Sequence -> String
sequenceToString (Sequence s gs) = s ++ concatMap unGroup gs

-- | A number string after radix and grouping characters have been
-- stripped out.
data NumberStr =
  Whole String
  -- ^ A whole number only. No radix point.
  | WholeRad String
    -- ^ A whole number and a radix point, but nothing after the radix
    -- point.
  | WholeRadFrac String String
    -- ^ A whole number and something after the radix point.
  | RadFrac String
    -- ^ A radix point and a fractional value after it, but nothing
    -- before the radix point.
  deriving Show

numberToStr :: Number -> NumberStr
numberToStr n = case n of
  NumberRadSeq s -> RadFrac (sequenceToString s)
  NumberSeqPost s mayPost -> case mayPost of
    Nothing -> Whole (sequenceToString s)
    Just (PostSequence maySeq) -> case maySeq of
      Nothing -> WholeRad (sequenceToString s)
      Just pss -> WholeRadFrac (sequenceToString s)
                  (sequenceToString pss)

-- | Do not use Prelude.read or Prelude.reads on whole decimal strings
-- like @232.72@. Sometimes it will fail, though sometimes it will
-- succeed; why is not clear to me. Hopefully reading integers won't
-- fail! However, in case it does, use read', whose error message will
-- at least tell you what number was being read.
--
-- Data.Decimal cannot handle decimals whose exponent would exceed
-- 255, which is the maximum that a Word8 can hold. A Word8 is used to
-- hold the exponent. If the exponent would exceed 255, this function
-- fails.
toDecimal :: NumberStr -> Maybe D.Decimal
toDecimal ns = case ns of
  Whole s -> Just $ D.Decimal 0 (readWithErr s)
  WholeRad s -> Just $ D.Decimal 0 (readWithErr s)
  WholeRadFrac w f -> fromWholeRadFrac w f
  RadFrac f -> fromWholeRadFrac "0" f
  where
    fromWholeRadFrac w f = let
      len = length f
      in if len > 255
         then Nothing
         else Just $ D.Decimal (fromIntegral len) (readWithErr (w ++ f))

readWithErr :: String -> Integer
readWithErr s = let
  readSresult = reads s
  in case readSresult of
    (i, ""):[] -> i
    _ -> error $ "readWithErr failed. String being read: " ++ s
         ++ " Result of reads: " ++ show readSresult

qty :: Parser Qty
qty = do
  num <- fmap numberToStr parseNumber
  d <- case toDecimal num of
    Nothing -> fail "fractional part too big"
    Just dec -> return dec
  case newQty d of
    Nothing -> fail $ "could not create quantity; zero quantities "
                    ++ "are not allowed: " ++ show d
    Just q -> return q

-- | Specifies how to perform digit grouping when rendering a
-- quantity. All grouping groups into groups of 3 digits.
data GroupingSpec =
  NoGrouping
  -- ^ Do not perform any digit grouping
  | GroupLarge
    -- ^ Group digits, but only if the number to be grouped is greater
    -- than 9,999 (if grouping the whole part) or if there are more
    -- than 4 decimal places (if grouping the fractional part).
  | GroupAll
    -- ^ Group digits whenever there are at least four decimal places.
  deriving (Eq, Show)

-- | Renders an unquoted Qty. Performs digit grouping as requested.
render
  :: (GroupingSpec, GroupingSpec)
  -- ^ Group for the portion to the left and right of the radix point?

  -> Qty
  -> X.Text
render (gl, gr) q = let
  qs = show . unQty $ q
  in X.pack $ case splitOn "." qs of
    w:[] -> groupWhole gl w
    w:d:[] ->
      groupWhole gl w ++ renderRadix ++ groupDecimal gr d
    _ -> error "Qty.hs: rendering error"

renderGrouper :: String
renderGrouper = "\x2009"

renderRadix :: String
renderRadix = "."


-- | Performs grouping for amounts to the left of the radix point.
groupWhole :: GroupingSpec -> String -> String
groupWhole gs o = let
  grouped = intercalate renderGrouper
            . reverse
            . map reverse
            . splitEvery 3
            . reverse
            $ o
  in case gs of
  NoGrouping -> o
  GroupLarge -> if length o > 4 then grouped else o
  GroupAll -> grouped

-- | Performs grouping for amounts to the right of the radix point.
groupDecimal :: GroupingSpec -> String -> String
groupDecimal gs o = let
  grouped = intercalate renderGrouper
            . splitEvery 3
            $ o
  in case gs of
    NoGrouping -> o
    GroupLarge -> if length o > 4 then grouped else o
    GroupAll -> grouped

module Penny.Copper.Commodity where

import Control.Applicative ((<*>), (<$>), pure, (*>))
import qualified Data.Char as C
import Data.Char (
  isControl, isSpace, isDigit)
import Data.Text ( pack )
import Text.Parsec ( satisfy, many, char, sepBy1, many1, (<?>),
                     between)
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import Data.List.NonEmpty (nonEmpty, unsafeToNonEmpty)
import Penny.Copper.Util (inCat)
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

quotedCmdtyChar :: Char -> Bool
quotedCmdtyChar c = (category || specific) && notBanned where
  category = inCat C.UppercaseLetter C.OtherSymbol c
  specific = c == ' '
  notBanned = not $ c `elem` ['"', ':']

quotedSubCmdty :: Parser B.SubCommodity
quotedSubCmdty = f <$> m <?> "sub commodity" where
  m = many1 (satisfy quotedCmdtyChar)
  f cs = B.SubCommodity (TextNonEmpty (head cs) (pack $ tail cs))

quotedCmdty :: Parser B.Commodity
quotedCmdty = between quot quot c where
  quot = char '"'
  c = (B.Commodity . unsafeToNonEmpty)
      <$> sepBy1 quotedSubCmdty (char ':')

isCommodityChar :: Char -> Bool
isCommodityChar c = and $ (map (not .) ps) <*> pure c where
  ps = [isControl, isSpace, (== ':')]

isNonDigitChar :: Char -> Bool
isNonDigitChar c = and $ [not . isDigit, isCommodityChar] <*> pure c

firstSubWithDigits :: Parser B.SubCommodity
firstSubWithDigits = do
  c <- satisfy isNonDigitChar
  rs <- many $ satisfy isCommodityChar
  return (B.SubCommodity (TextNonEmpty c (pack rs)))

nextSubWithDigits :: Parser B.SubCommodity
nextSubWithDigits = do
  let p = satisfy isCommodityChar
  c <- p
  rs <- many p
  return (B.SubCommodity (TextNonEmpty c (pack rs)))

commodityWithDigits :: Parser B.Commodity
commodityWithDigits = do
  f <- firstSubWithDigits
  rs <- many $ do
    _ <- char ':'
    nextSubWithDigits
  return (B.Commodity (nonEmpty f rs))

firstSubNoDigits :: Parser B.SubCommodity
firstSubNoDigits = do
  c <- satisfy isNonDigitChar
  rs <- many $ satisfy isNonDigitChar
  return (B.SubCommodity (TextNonEmpty c (pack rs)))

nextSubNoDigits :: Parser B.SubCommodity
nextSubNoDigits = do
  let p = satisfy isNonDigitChar
  c <- p
  rs <- many p
  return (B.SubCommodity (TextNonEmpty c (pack rs)))

commodityNoDigits :: Parser B.Commodity
commodityNoDigits = do
  f <- firstSubNoDigits
  rs <- many $ do
    _ <- char ':'
    nextSubNoDigits
  return (B.Commodity (nonEmpty f rs))

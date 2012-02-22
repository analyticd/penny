module Penny.Copper.Commodity where

import Data.Char (
  isControl, isSpace, isDigit)
import Data.Text ( pack )
import qualified Data.Traversable as T
import Text.Parsec ( satisfy, many, char )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import Data.List.NonEmpty (nonEmpty)
import Penny.Lincoln.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )

isCommodityChar :: Char -> Bool
isCommodityChar = and . T.sequenceA (map (not .) ps) where
  ps = [isControl, isSpace, (== ':')]

isNonDigitChar :: Char -> Bool
isNonDigitChar = and . T.sequenceA [not . isDigit, isCommodityChar]

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

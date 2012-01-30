module Penny.Parser.Commodity where

import Data.Char ( generalCategory, isLetter,
                   GeneralCategory (CurrencySymbol),
                   isNumber, isControl, isSpace, isDigit)
import Data.Monoid ( mconcat, mappend )
import Data.Monoid.Extra ( All (All, appAll ) )
import Data.Text ( pack )
import Text.Parsec ( satisfy, many, char )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits.Commodity as C
import Penny.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ) )
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )


isCommodityChar :: Char -> Bool
isCommodityChar =
  appAll . mconcat . map All . map (not .)
  $ [isControl, isSpace, (== ':')]

isNonDigitChar :: Char -> Bool
isNonDigitChar = appAll (mappend f g) where
  f = All $ not . isDigit
  g = All isCommodityChar

firstSubWithDigits :: Parser C.SubCommodity
firstSubWithDigits = do
  c <- satisfy isNonDigitChar
  rs <- many $ satisfy isCommodityChar
  return (C.SubCommodity (TextNonEmpty c (pack rs)))

nextSubWithDigits :: Parser C.SubCommodity
nextSubWithDigits = do
  let p = satisfy isCommodityChar
  c <- p
  rs <- many p
  return (C.SubCommodity (TextNonEmpty c (pack rs)))

commodityWithDigits :: Parser C.Commodity
commodityWithDigits = do
  f <- firstSubWithDigits
  rs <- many $ do
    _ <- char ':'
    nextSubWithDigits
  return (C.Commodity (AtLeast1 f rs))

firstSubNoDigits :: Parser C.SubCommodity
firstSubNoDigits = do
  c <- satisfy isNonDigitChar
  rs <- many $ satisfy isNonDigitChar
  return (C.SubCommodity (TextNonEmpty c (pack rs)))

nextSubNoDigits :: Parser C.SubCommodity
nextSubNoDigits = do
  let p = satisfy isNonDigitChar
  c <- p
  rs <- many p
  return (C.SubCommodity (TextNonEmpty c (pack rs)))

commodityNoDigits :: Parser C.Commodity
commodityNoDigits = do
  f <- firstSubNoDigits
  rs <- many $ do
    _ <- char ':'
    nextSubNoDigits
  return (C.Commodity (AtLeast1 f rs))

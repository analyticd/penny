module Penny.Parser.Commodity where

import Data.Char ( generalCategory, isLetter,
                   GeneralCategory (CurrencySymbol),
                   isNumber )
import Data.Text ( pack )
import Text.Parsec ( satisfy, many, char )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits.Commodity as C
import Penny.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ) )
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )


commoditySymbol :: Parser C.Commodity
commoditySymbol = do
  let p ch = generalCategory ch == CurrencySymbol
  c <- satisfy p
  return $ C.charCommodity c

subCommodity :: Parser C.SubCommodity
subCommodity = do
  c <- satisfy isLetter
  rs <- many $ satisfy (\l -> isLetter l || isNumber l)
  return (C.SubCommodity (TextNonEmpty c (pack rs)))

commodityLong :: Parser C.Commodity
commodityLong = do
  f <- subCommodity
  rs <- many $ do
    _ <- char ':'
    subCommodity
  return (C.Commodity (AtLeast1 f rs))


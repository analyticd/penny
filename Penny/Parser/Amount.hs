module Penny.Parser.Amount where

import Control.Applicative ((<*>), pure )
import Control.Monad ( void )
import Text.Parsec ( (<|>), char, choice, try )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits.Amount as A
import qualified Penny.Bits.Commodity as C
import qualified Penny.Parser.Commodity as CP
import qualified Penny.Parser.Qty as Q
import qualified Penny.Reports as R

commoditySpaceQty ::
  Q.Radix
  -> Q.Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
commoditySpaceQty rdx sep = do
  c <- CP.commoditySymbol <|> CP.commodityLong
  void $ char ' '
  q <- Q.qty rdx sep
  let fmt = R.CommodityFmt R.CommodityOnLeft R.SpaceBetween
  return (A.Amount q c, (c, fmt))

commodityQty ::
  Q.Radix
  -> Q.Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
commodityQty rdx sep = do
  c <- CP.commoditySymbol
  q <- Q.qty rdx sep
  let fmt = R.CommodityFmt R.CommodityOnLeft R.NoSpaceBetween
  return (A.Amount q c, (c, fmt))

qtyCommodity ::
  Q.Radix
  -> Q.Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
qtyCommodity rdx sep = do
  q <- Q.qty rdx sep
  c <- CP.commoditySymbol
  let fmt = R.CommodityFmt R.CommodityOnRight R.NoSpaceBetween
  return (A.Amount q c, (c, fmt))

qtySpaceCommodity ::
  Q.Radix
  -> Q.Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
qtySpaceCommodity rdx sep = do
  q <- Q.qty rdx sep
  void $ char ' '
  c <- CP.commoditySymbol <|> CP.commodityLong
  let fmt = R.CommodityFmt R.CommodityOnRight R.SpaceBetween
  return (A.Amount q c, (c, fmt))

amount ::
  Q.Radix
  -> Q.Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
amount rdx sep =
  choice
  . map try
  $ [commoditySpaceQty, commodityQty,
     qtyCommodity, qtySpaceCommodity]
  <*> pure rdx
  <*> pure sep


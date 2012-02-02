module Penny.Copper.Amount where

import Control.Applicative ((<*>), pure )
import Control.Monad ( void )
import Text.Parsec ( char, choice, try, many )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Commodity as C
import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Meta as M

spaces :: Parser ()
spaces = void (many (char ' '))

commoditySpaceQty ::
  Q.Radix
  -> Q.Separator
  -> Parser (B.Amount, M.Format)
commoditySpaceQty rdx sep = do
  c <- C.commodityWithDigits
  void $ char ' '
  spaces
  q <- Q.qty rdx sep
  let fmt = M.Format c M.CommodityOnLeft M.SpaceBetween
  return (B.Amount q c, fmt)

commodityQty ::
  Q.Radix
  -> Q.Separator
  -> Parser (B.Amount, M.Format)
commodityQty rdx sep = do
  c <- C.commodityNoDigits
  q <- Q.qty rdx sep
  let fmt = M.Format c M.CommodityOnLeft M.NoSpaceBetween
  return (B.Amount q c, fmt)

qtyCommodity ::
  Q.Radix
  -> Q.Separator
  -> Parser (B.Amount, M.Format)
qtyCommodity rdx sep = do
  q <- Q.qty rdx sep
  c <- C.commodityWithDigits
  let fmt = M.Format c M.CommodityOnRight M.NoSpaceBetween
  return (B.Amount q c, fmt)

qtySpaceCommodity ::
  Q.Radix
  -> Q.Separator
  -> Parser (B.Amount, M.Format)
qtySpaceCommodity rdx sep = do
  q <- Q.qty rdx sep
  void $ char ' '
  spaces
  c <- C.commodityWithDigits
  let fmt = M.Format c M.CommodityOnRight M.SpaceBetween
  return (B.Amount q c, fmt)

amount ::
  Q.Radix
  -> Q.Separator
  -> Parser (B.Amount, M.Format)
amount rdx sep =
  choice
  . map try
  $ [commodityQty, commoditySpaceQty,
     qtyCommodity, qtySpaceCommodity]
  <*> pure rdx
  <*> pure sep


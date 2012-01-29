module Penny.Parser.Amount where

import qualified Penny.Bits.Commodity as C
import qualified Penny.Reports as R

commoditySpaceQty ::
  Radix
  -> Separator
  -> Parser (A.Amount, (C.Commodity, R.CommodityFmt))
commoditySpaceQty rdx sep = do
  c <- commoditySymbol <|> commodityLong
  void $ char ' '
  q <- qty rdx sep
  let fmt = R.CommodityFmt R.CommodityOnLeft R.SpaceBetween
  return (A.Amount q c, (c, fmt))



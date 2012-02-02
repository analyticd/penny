module Penny.Copper.Price where

import Control.Monad ( void )
import Text.Parsec ( char, many, getPosition, sourceLine )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Boxes as Box
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Amount as A
import qualified Penny.Copper.Commodity as C
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Meta as M
import qualified Penny.Copper.Qty as Q

whitespace :: Parser ()
whitespace = void (many (char ' '))

price ::
  DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser (Box.PriceBox M.PriceMeta)
price dtz rad sep = do
  void $ char 'P'
  pos <- getPosition
  whitespace
  dt <- DT.dateTime dtz
  whitespace
  com <- C.commodityWithDigits
  whitespace
  (amt, fmt) <- A.amount rad sep
  let (from, to) = (B.From com, B.To (B.commodity amt))
      cpu = B.CountPerUnit (B.qty amt)
      lin = M.PriceLine . M.Line . sourceLine $ pos
      meta = M.PriceMeta lin fmt
  pr <- case B.newPrice from to cpu of
    (Just pri) -> return pri
    Nothing -> fail "invalid price given"
  return $ Box.PriceBox (B.PricePoint dt pr) (Just meta)

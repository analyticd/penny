module Penny.Copper.Price (price, render) where

import Text.Parsec ( char, getPosition, sourceLine, (<?>),
                     SourcePos )
import Text.Parsec.Text ( Parser )

import Control.Applicative ((<*>), (<*), (<|>), (<$))
import Data.Text (singleton, snoc, intercalate)
import qualified Data.Text as X

import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Amount as A
import qualified Penny.Copper.Commodity as C
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Util (lexeme, eol)

{-
BNF-style specification for prices:

<price> ::= "dateTime" <fromCmdty> <toAmount>
<fromCmdty> ::= "quotedLvl1Cmdty" | "lvl2Cmdty"
<toAmount> ::= "amount"
-}

mkPrice :: SourcePos
         -> L.DateTime
         -> L.Commodity
         -> (L.Amount, L.Format)
         -> Maybe L.PricePoint
mkPrice pos dt from (am, fmt) = let
  to = L.commodity am
  q = L.qty am
  pm = L.PriceMeta (Just pl) (Just fmt)
  pl = L.PriceLine . sourceLine $ pos
  in do
    p <- L.newPrice (L.From from) (L.To to) (L.CountPerUnit q)
    return $ L.PricePoint dt p pm

maybePrice :: Parser (Maybe L.PricePoint)
maybePrice =
  mkPrice
  <$ lexeme (char '@')
  <*> getPosition
  <*> lexeme DT.dateTime
  <*> lexeme (C.quotedLvl1Cmdty <|> C.lvl2Cmdty)
  <*> A.amount
  <* eol
  <?> "price"

-- | A price with an EOL and whitespace after the EOL. @price d r@
-- will parse a PriceBox, where @d@ is the DefaultTimeZone for the
-- DateTime in the PricePoint, and @r@ is the radix point and grouping
-- character to parse the Amount. Fails if the price is not valid
-- (e.g. the from and to commodities are the same).
price :: Parser L.PricePoint
price = do
  p <- maybePrice
  case p of
    Nothing -> fail "invalid price given"
    Just pr -> return pr


-- | @render dtz rg f pp@ renders a price point @pp@. @dtz@ is the
-- DefaultTimeZone for rendering of the DateTime in the Price
-- Point. @rg@ is the radix point and grouping character for the
-- amount. @f@ is the Format for how the price will be
-- formatted. Fails if either the From or the To commodity cannot be
-- rendered.
render ::
  (Q.GroupingSpec, Q.GroupingSpec)
  -> L.PricePoint
  -> Maybe X.Text
render gs pp = let
  dateTxt = DT.render (B.dateTime pp)
  (L.From from) = L.from . L.price $ pp
  (L.To to) = L.to . L.price $ pp
  (L.CountPerUnit q) = L.countPerUnit . L.price $ pp
  mayFromTxt = C.renderLvl3 from <|> C.renderQuotedLvl1 from
  amt = L.Amount q to
  in do
    fmt <- L.priceFormat . L.ppMeta $ pp
    let mayAmtTxt = A.render gs fmt amt
    amtTxt <- mayAmtTxt
    fromTxt <- mayFromTxt
    return $
       (intercalate (singleton ' ')
       [singleton '@', dateTxt, fromTxt, amtTxt])
       `snoc` '\n'
